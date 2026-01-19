package com.verr1.controlcraft.unstable.management;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.vsapi.ShipAssembler;
import com.verr1.controlcraft.unstable.AIServer;
import com.verr1.controlcraft.unstable.blocks.AiBoundFakePlayer;
import com.verr1.controlcraft.unstable.data.AIPersistentData;
import com.verr1.controlcraft.unstable.data.schematic.AISchematic;
import com.verr1.controlcraft.unstable.data.schematic.SchematicKey;
import com.verr1.controlcraft.unstable.util.LazyTicker;
import com.verr1.controlcraft.unstable.valkyrienskies.attachments.AIBlockNetwork;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.MinecraftUtils;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import kotlin.Pair;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.saveddata.SavedData;
import org.jetbrains.annotations.NotNull;
import org.joml.Quaterniond;
import org.joml.Quaterniondc;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.joml.primitives.AABBi;
import org.joml.primitives.AABBic;
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.core.apigame.world.ServerShipWorldCore;
import org.valkyrienskies.core.impl.game.ShipTeleportDataImpl;
import org.valkyrienskies.core.impl.game.ships.ShipDataCommon;
import org.valkyrienskies.mod.common.BlockStateInfo;
import org.valkyrienskies.mod.common.VSGameUtilsKt;

import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public class AIPool extends SavedData {

    private static final Serializer<Map<Long, AIPersistentData>> PERSISTENT = SerializeUtils.ofMap(
            SerializeUtils.LONG,
            AIPersistentData.SER
    );

    public static final String DATA_NAME = ControlCraft.MODID + "_ai_pool";
    public static Vector3d SIMPLE_YARD_POSITION = new Vector3d(0, -64, 0);
    public static Vector3d SIMPLE_CREATE_POSITION = new Vector3d(0, 96, 0);
    private final Map<Long, AIPersistentData> persistent = new HashMap<>();


    private final AIYardAllocator allocator = new AIYardAllocator.Simple(this::yard);
    private final Map<Long, Pair<Long, Vector3dc>> availableAIAllocatePointer = new HashMap<>();
    private final Set<Long> availableAI = new HashSet<>();


    private final LazyTicker lazyValidator = new LazyTicker(100, this::validate);
    private final LazyTicker lazyYardGuard = new LazyTicker(60, this::ensureStatic);
    private final LazyTicker lazyAttTicker = new LazyTicker(10, this::tickAttachment);
    private final LazyTicker lazyFreeCacher = new LazyTicker(100, this::tickFreeAiCountCache);

    private int cachedFreeAi = 0;

    private Vector3dc yard(){
        return SIMPLE_YARD_POSITION;
    }




    public void reset(){
        availableAI.clear();
        availableAIAllocatePointer.clear();
        allocator.clear();
        persistent.keySet().forEach(this::discard);
        setDirty();
    }

    public void resetAlive(){
        getFreeAI().forEach(this::discard);
    }

    private void clearAll(){
        availableAI.clear();
        availableAIAllocatePointer.clear();
        allocator.clear();
        persistent.clear();
        setDirty();
    }

    public void tick(){
        lazyValidator.tick();
        lazyYardGuard.tick();
        lazyAttTicker.tick();
        lazyFreeCacher.tick();
    }

    public void onServerStarted(){
        ControlCraftServer.SERVER_EXECUTOR.executeLater(this::reset, 20);
    }

    private MinecraftServer server(){
        return Objects.requireNonNull(ControlCraftServer.INSTANCE);
    }

    public static String randomSequence(int length){
        StringBuilder sb = new StringBuilder(length);
        Random random = new Random();
        for (int i = 0; i < length; i++) {
            sb.append((char) ('a' + random.nextInt(26))); // 'a' to 'z'
        }
        return sb.toString();
    }

    public @NotNull String dimensionOf(long id){
        return getShipOf(id).map(ServerShip::getChunkClaimDimension).orElse("null");
    }

    public Optional<ServerLevel> getLevelOf(long id){
        return Optional.ofNullable(VSGameUtilsKt.getLevelFromDimensionId(server(), dimensionOf(id)));
    }

    private ServerShipWorldCore vsWorld(){
        return Objects.requireNonNull(VSGameUtilsKt.getShipObjectWorld(server()));
    }

    public @NotNull List<ServerShip> getAllShips(){
        return vsWorld().getAllShips().stream().toList();
    }

    public Optional<ServerShip> getShipOf(long id){
        return Optional.ofNullable(vsWorld().getAllShips().getById(id));
    }

    public AIBlockNetwork getNetworkOf(long id){
        return networkOf(id).orElse(null);
    }

    public Optional<AIPersistentData> getDataOf(long id){
        return Optional.ofNullable(persistent.get(id));
    }

    public Optional<ServerShip> getShipAt(WorldBlockPos pos){
        return Optional
                .ofNullable(
                        VSGameUtilsKt
                                .getShipManagingPos(
                                        pos.level(server()),
                                        pos.pos()
                                )
                );
    }

    public List<Long> listAvailableAI(){
        return availableAI.stream().toList();
    }

    public List<Long> listFreeAI(){
        return getFreeAI().stream().toList();
    }

    public List<Long> listAllAI(){
        return persistent.keySet().stream().toList();
    }

    private void remove(long i){
        persistent.remove(i);
        availableAI.remove(i);
        if(availableAIAllocatePointer.containsKey(i)){
            allocator.free(availableAIAllocatePointer.get(i).getFirst());
            availableAIAllocatePointer.remove(i);
        }
        setDirty();
    }

    public void validate(){
        persistent.keySet().stream().filter(i -> getShipOf(i).isEmpty()).toList().forEach(this::remove);
    }

    private static void logAbsentId(long id){
        ControlCraft.LOGGER.error(
                "Tried to access AI of ship with id {}, but no ship with that id exists in the world. ",
                id
        );
    }

    private static void logAbsentSchematic(SchematicKey key){
        ControlCraft.LOGGER.error(
                "Tried to access AI with schematic key {}, but no such schematic exists.",
                key
        );
    }

    public void markAsAI(long id){
        getShipOf(id).ifPresentOrElse(
                s -> {
                    AIBlockNetwork.getOrCreate(s);
                    s.setSlug("ai_" + id);
                },
                () -> logAbsentId(id)
        );
    }

    public void unMarkAI(long id){
        getShipOf(id).ifPresentOrElse(s -> {
            s.saveAttachment(AIBlockNetwork.class, null);
            s.setSlug("unmarked_ai_" + randomSequence(5));
        }, () -> logAbsentId(id));
    }

    public boolean isAI(long id){
        Optional<ServerShip> opt = getShipOf(id);
        return opt.filter(ship -> ship.getAttachment(AIBlockNetwork.class) != null).isPresent();
    }

    public boolean isAIQuickTest(long id){
        return persistent.containsKey(id);
    }

    public boolean isInPool(long id){
        return availableAI.contains(id);
    }

    public RepairResult repairAI(long id, @NotNull SchematicKey overrideKey){
        AIPersistentData data = persistent.get(id);
        if(data == null){
            logAbsentId(id);
            MinecraftUtils.broadcastMessage(Component.literal("Ship with id: " + id + " is not recorded as AI!"));
            return RepairResult.SHIP_AI_NOT_RECORDED;
        }
        AISchematic schematic = AIServer.SCHEMATICS_MANAGER.getLoaded(overrideKey);
        if(schematic == null){
            logAbsentSchematic(overrideKey);
            MinecraftUtils.broadcastMessage(Component.literal("AI with type: " + overrideKey + " has no schematic loaded!"));
            return RepairResult.SCHEMATIC_NOT_LOADED;
        }
        BlockPos center = data.center;

        ServerLevel level = getLevelOf(id).orElse(null);

        if(level == null){
            ControlCraft.LOGGER.error("Tried to repair AI with id {}, but no level found for the ship.", id);
            return RepairResult.CANNOT_ACCESS_LEVEL;
        }
        schematic.repairAt(center, level);
        return RepairResult.SUCCESS;
    }

    public void restoreAI(long id){
        repairAI(id, persistent.get(id).storageSchematic);
    }

    private AIBlockNetwork networkOf(ServerShip s){
        return AIBlockNetwork.getOrCreate(s);
    }

    private Optional<AIBlockNetwork> networkOf(long s){
        return getShipOf(s).map(AIBlockNetwork::getOrCreate);
    }

    public void discard(long id){
        if(!isAI(id))return;
        ServerShip ship = getShipOf(id).orElse(null);
        if(ship == null)return;
        ServerLevel level = getLevelOf(id).orElse(null);
        if(level == null)return;

        ship.setStatic(true);

        networkOf(ship).onDiscard();

        if(availableAIAllocatePointer.containsKey(id)){
            allocator.free(availableAIAllocatePointer.get(id).getFirst());
        }

        Long spacePointer = allocator.allocate(Optional.ofNullable(ship.getShipAABB()).orElse(new AABBi()));
        Vector3dc yardPosition = allocator.position(spacePointer);
        availableAIAllocatePointer.put(ship.getId(), new Pair<>(spacePointer, yardPosition));
        availableAI.add(ship.getId());
        Runnable task = () -> {
            networkOf(ship).onPreRestore();
            restoreAI(id);
            restoreAI(id);
            networkOf(ship).onPostRestore();
        };
        task.run();
        vsWorld().teleportShip(ship, withPosition(yardPosition, ship.getChunkClaimDimension()));
        remass(level, ship);
        // ControlCraftServer.SERVER_EXECUTOR.executeLater(task, 1);
    }

    public @NotNull AISpawnResult spawn(long id, SchematicKey overrideKey ,Vector3dc position, Quaterniondc rotation, Vector3dc velocity, Vector3dc omega){
        ServerShip ship = getShipOf(id).orElse(null);
        if(ship == null)return AISpawnResult.DELETED;
        if(!isAI(id))return AISpawnResult.NOT_AN_AI;
        if(!availableAI.contains(id))return AISpawnResult.NOT_AVAILABLE;

        Long spacePointer = availableAIAllocatePointer.get(id).getFirst();
        allocator.free(spacePointer);
        availableAIAllocatePointer.remove(id);
        availableAI.remove(id);
        ship.setStatic(false);


        networkOf(ship).onPreRepair();
        RepairResult result = repairAI(id, overrideKey);

        if(result != RepairResult.SUCCESS){
            ControlCraft.LOGGER.error("Failed to repair AI ship with id {} during spawning. Abort spawn.", id);
            return new AISpawnResult(-1, AISpawnResult.Status.CAN_NOT_REPAIR, result);
        }

        networkOf(ship).onPostRepair();

        Runnable task = () -> {
            vsWorld().teleportShip(ship, withPose(position, rotation, velocity, omega, ship.getChunkClaimDimension()));
            networkOf(ship).onSpawn();
        };

        // ControlCraftServer.SERVER_EXECUTOR.executeLater(task, 2);
        task.run();

        return new AISpawnResult(id);
    }

    public @NotNull AISpawnResult spawn(SchematicKey type, Vector3dc position, Quaterniondc rotation, Vector3dc velocity, Vector3dc omega){
        AtomicReference<AISpawnResult> ref = new AtomicReference<>(AISpawnResult.USE_UP);

        availableAI.stream().findAny().ifPresent(
                id -> ref.set(spawn(id, type, position, rotation, velocity, omega))
        );

        return ref.get();
    }

    public @NotNull AISpawnResult spawn(SchematicKey type, Vector3dc position, Quaterniondc rotation){
        return spawn(type, position, rotation, new Vector3d(), new Vector3d());
    }

    public void joinPool(SchematicKey defaultSchematic, ServerLevel level){
        AISchematic schematic = AIServer.SCHEMATICS_MANAGER.getLoaded(defaultSchematic);
        if(schematic == null){
            logAbsentSchematic(defaultSchematic);
            return;
        }
        ServerShip newAIShip = create1Block(level);
        Vector3dc createdShipCenter = newAIShip.getInertiaData().getCenterOfMassInShip();
        BlockPos center = BlockPos.containing(toMinecraft(createdShipCenter));
        long id = newAIShip.getId();

        persistent.put(id, new AIPersistentData(center, defaultSchematic));
        availableAI.add(id);
        markAsAI(id);
        discard(newAIShip.getId());

        setDirty();
    }


    public static ServerShip create1Block(ServerLevel level){
        BlockPos createdWorldCenter = BlockPos.containing(toMinecraft(SIMPLE_CREATE_POSITION));
        level.setBlock(createdWorldCenter, Blocks.STONE.defaultBlockState(), 3);
        ServerShip newShip = ShipAssembler.INSTANCE.assembleToShip(level, createdWorldCenter, true, 1, true);

        return newShip;
    }

    public static ShipTeleportDataImpl withPosition(Vector3dc newPosition, String newDim){
        return new ShipTeleportDataImpl(
                newPosition,
                new Quaterniond(),
                new Vector3d(),
                new Vector3d(),
                newDim,
                1.0
        );
    }

    public Set<Long> getFreeAI(){
        return persistent.keySet().stream().filter(i -> !availableAI.contains(i)).collect(Collectors.toSet());
    }


    public void ensureStatic(){
        availableAI.forEach(i -> getShipOf(i).ifPresent(s -> {
            s.setStatic(true);
            Optional.ofNullable(availableAIAllocatePointer.get(i))
                    .map(Pair::getSecond)
                    .ifPresent(
                            p -> vsWorld().teleportShip(s, withPosition(
                                    p,
                                    s.getChunkClaimDimension()
                            ))
                    );
        }));
    }

    private void tickFreeAiCountCache(){
        cachedFreeAi = listFreeAI().size();
    }

    public void tickAttachment(){
        persistent
                .keySet()
                .stream()
                .map(i -> getShipOf(i).orElse(null))
                .filter(Objects::nonNull)
                .forEach(ship -> {
                    AIBlockNetwork network = AIBlockNetwork.getOrCreate(ship);
                    network.getOrCreateFakePlayer(() -> new AiBoundFakePlayer(ship));
                    network.tick();
                });
    }


    public static ShipTeleportDataImpl withPose(Vector3dc newPosition, Quaterniondc newRotation, String newDim){
        return withPose(newPosition, newRotation, new Vector3d(), new Vector3d(), newDim);
    }

    public static ShipTeleportDataImpl withPose(
            Vector3dc newPosition,
            Quaterniondc newRotation,
            Vector3dc vel,
            Vector3dc omg,
            String newDim
    ){
        return new ShipTeleportDataImpl(newPosition, newRotation, vel, omg, newDim, 1.0);
    }

    public void setYardPosition(double x, double y, double z){
        SIMPLE_YARD_POSITION.set(x, y, z);
        setDirty();
    }

    @Override
    public @NotNull CompoundTag save(@NotNull CompoundTag tag) {
        tag.put("ai_pool", serialize());
        return tag;
    }


    public CompoundTag serialize(){
        return CompoundTagBuilder.create()
                .withCompound("database", PERSISTENT.serialize(persistent))
                .withCompound("yard", SerializeUtils.VECTOR3D.serialize(SIMPLE_YARD_POSITION))
                .build();
    }

    public void deserialize(CompoundTag tag){
        availableAI.clear();
        availableAIAllocatePointer.clear();
        allocator.clear();
        persistent.clear();
        persistent.putAll(PERSISTENT.deserialize(tag.getCompound("database")));
        SIMPLE_YARD_POSITION = SerializeUtils.VECTOR3D.deserialize(tag.getCompound("yard"));
    }

    private static AIPool load(@NotNull CompoundTag tag) {
        AIPool saved = new AIPool();
        saved.deserialize(tag.getCompound("ai_pool"));
        return saved;
    }


    public static AIPool load(MinecraftServer server){
        return server.overworld().getDataStorage().computeIfAbsent(AIPool::load, AIPool::new, DATA_NAME);
    }

    public static boolean remass(ServerLevel level, ServerShip ship){
        AABBic aabb = ship.getShipAABB();
        if(aabb == null)return false;
        boolean wasStatic = ship.isStatic();
        ship.setStatic(true);
        var airBlockMass_airBlockType = BlockStateInfo.INSTANCE.get(Blocks.AIR.defaultBlockState());
        var solidBlockMass_solidBlockType = BlockStateInfo.INSTANCE.get(Blocks.STONE.defaultBlockState());
        if(airBlockMass_airBlockType == null || solidBlockMass_solidBlockType == null)return false;
        ServerShipWorldCore shipWorld = VSGameUtilsKt.getShipObjectWorld(level);

        var airBlockType = airBlockMass_airBlockType.getSecond();
        BlockPos.betweenClosed(
            aabb.minX(), aabb.minY(), aabb.minZ(),
            aabb.maxX(), aabb.maxY(), aabb.maxZ()
        ).forEach( it -> {
            var state = level.getBlockState(it);
            var blockMass_blockType = BlockStateInfo.INSTANCE.get(state);
            if(blockMass_blockType == null)return;
            var blockType = blockMass_blockType.getSecond();
            if(!blockType.equals(airBlockType)){
                shipWorld.onSetBlock(
                    it.getX(), it.getY(), it.getZ(),
                    ship.getChunkClaimDimension(),
                    blockType, airBlockType,
                    0.0, 0.0
                );
            }
        });

//        ((ShipDataCommon)ship).setShipAABB();
        // var solidBlockType = solidBlockMass_solidBlockType.getSecond();
        shipWorld.onSetBlock(
            aabb.minX(), aabb.minY(), aabb.minZ(),
            ship.getChunkClaimDimension(),
            airBlockType, airBlockType,
            ship.getInertiaData().getMass(), 0.0
        );

        BlockPos.betweenClosed(
            aabb.minX(), aabb.minY(), aabb.minZ(),
            aabb.maxX(), aabb.maxY(), aabb.maxZ()
        ).forEach( it -> {
            var state = level.getBlockState(it);
            var blockMass_blockType = BlockStateInfo.INSTANCE.get(state);
            if(blockMass_blockType == null)return;
            var blockType = blockMass_blockType.getSecond();
            var blockMass = blockMass_blockType.getFirst();
            if(!blockType.equals(airBlockType)){
                shipWorld.onSetBlock(
                    it.getX(), it.getY(), it.getZ(),
                    ship.getChunkClaimDimension(),
                    airBlockType, blockType,
                    0.0, blockMass
                );
            }
        });
        ship.setStatic(wasStatic);
        return true;
    }

}
