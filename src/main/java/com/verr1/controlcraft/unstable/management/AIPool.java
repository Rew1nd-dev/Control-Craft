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
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.core.apigame.world.ServerShipWorldCore;
import org.valkyrienskies.core.impl.game.ShipTeleportDataImpl;
import org.valkyrienskies.mod.common.VSGameUtilsKt;

import java.util.*;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public class AIPool extends SavedData {
    private static final Serializer<Map<Long, AIPersistentData>> PERSISTENT = SerializeUtils.ofMap(
            SerializeUtils.LONG,
            AIPersistentData.SER
    );

    private static final Serializer<Map<Long, Long>> POINTERS = SerializeUtils.ofMap(
            SerializeUtils.LONG,
            SerializeUtils.LONG
    );


    public static final String DATA_NAME = ControlCraft.MODID + "_ai_pool";
    public static Vector3d SIMPLE_YARD_POSITION = new Vector3d(0, 128, 0);

    public static Vector3d SIMPLE_CREATE_POSITION = new Vector3d(0, 96, 0);


    private final Map<Long, AIPersistentData> persistent = new HashMap<>();

    private final AIYardAllocator allocator = new AIYardAllocator.Simple(this::yard);

    private final Map<Long, Pair<Long, Vector3dc>> availableAIAllocatePointer = new HashMap<>();

    private final Set<Long> availableAI = new HashSet<>();

    private final LazyTicker lazyValidator = new LazyTicker(100, this::validate);
    private final LazyTicker lazyYardGuard = new LazyTicker(60, this::ensureStatic);
    private final LazyTicker lazyAttTicker = new LazyTicker(10, this::tickAttachment);

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

    public SchematicKey getTypeOf(long id){
        return persistent.getOrDefault(id, new AIPersistentData(BlockPos.ZERO, SchematicKey.EMPTY)).key;
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

    public void repairAI(long id){
        AIPersistentData data = persistent.get(id);
        if(data == null){
            logAbsentId(id);
            MinecraftUtils.broadcastMessage(Component.literal("Ship with id: " + id + " is not recorded as AI!"));
            return;
        }
        AISchematic schematic = AIServer.SCHEMATICS_MANAGER.getLoaded(data.key);
        if(schematic == null){
            logAbsentSchematic(data.key);
            MinecraftUtils.broadcastMessage(Component.literal("AI with type: " + data.key + " has no schematic loaded!"));
            return;
        }
        BlockPos center = data.center;

        ServerLevel level = getLevelOf(id).orElse(null);

        if(level == null){
            ControlCraft.LOGGER.error("Tried to repair AI with id {}, but no level found for the ship.", id);
            return;
        }

        schematic.repairAt(center, level);
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
        ship.setStatic(true);

        networkOf(ship).onDiscard();

        if(availableAIAllocatePointer.containsKey(id)){
            allocator.free(availableAIAllocatePointer.get(id).getFirst());
        }

        Long spacePointer = allocator.allocate(ship.getShipAABB());
        Vector3dc yardPosition = allocator.position(spacePointer);
        availableAIAllocatePointer.put(ship.getId(), new Pair<>(spacePointer, yardPosition));
        availableAI.add(ship.getId());



        vsWorld().teleportShip(ship, withPosition(yardPosition, ship.getChunkClaimDimension()));

        networkOf(ship).onPreRepair();
        repairAI(id);
        networkOf(ship).onPostRepair();
    }



    public void spawn(long id, Vector3dc position, Quaterniondc rotation){
        spawn(id, position, rotation, new Vector3d(), new Vector3d());
    }

    public void spawn(long id, Vector3dc position, Quaterniondc rotation, Vector3dc velocity, Vector3dc omega){
        if(!isAI(id))return;
        if(!availableAI.contains(id))return;
        ServerShip ship = getShipOf(id).orElse(null);
        if(ship == null)return;

        // repairAI(id); // repair twice in case of any exceptions

        Long spacePointer = availableAIAllocatePointer.get(id).getFirst();
        allocator.free(spacePointer);
        availableAIAllocatePointer.remove(id);
        availableAI.remove(id);
        ship.setStatic(false);
        vsWorld().teleportShip(ship, withPose(position, rotation, velocity, omega, ship.getChunkClaimDimension()));

        // ControlCraftServer.SERVER_EXECUTOR.executeLater(() -> ship.setStatic(false), 5);

        networkOf(ship).onSpawn();
    }

    public @NotNull AISpawnResult spawn(SchematicKey type, Vector3dc position, Quaterniondc rotation, Vector3dc velocity, Vector3dc omega){
        AtomicLong resultId = new AtomicLong(-1L);
        availableAI
                .stream()
                .filter(id -> persistent.get(id).key.equals(type))
                .findFirst()
                .ifPresent(
                    id -> {
                        spawn(id, position, rotation, velocity, omega);
                        resultId.set(id);
                    }
        );
        return new AISpawnResult(resultId.get());
    }

    public @NotNull AISpawnResult spawn(SchematicKey type, Vector3dc position, Quaterniondc rotation){
        AtomicLong resultId = new AtomicLong(-1L);
        availableAI
                .stream()
                .filter(id -> persistent.get(id).key.equals(type))
                .findFirst()
                .ifPresent(
                        id -> {
                            spawn(id, position, rotation);
                            resultId.set(id);
                        }
                );
        return new AISpawnResult(resultId.get());
    }

    public void joinPool(SchematicKey newAI, ServerLevel level){
        AISchematic schematic = AIServer.SCHEMATICS_MANAGER.getLoaded(newAI);
        if(schematic == null){
            logAbsentSchematic(newAI);
            return;
        }
        ServerShip newAIShip = create1Block(level);
        Vector3dc createdShipCenter = newAIShip.getInertiaData().getCenterOfMassInShip();
        BlockPos center = BlockPos.containing(toMinecraft(createdShipCenter));
        long id = newAIShip.getId();

        persistent.put(id, new AIPersistentData(center, newAI));
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

}
