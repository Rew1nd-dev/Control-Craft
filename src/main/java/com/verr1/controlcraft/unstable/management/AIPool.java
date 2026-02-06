package com.verr1.controlcraft.unstable.management;

import com.mojang.datafixers.util.Either;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.executor.executables.ConditionExecutable;
import com.verr1.controlcraft.foundation.managers.ChunkLoader;
import com.verr1.controlcraft.unstable.AIServer;
import com.verr1.controlcraft.unstable.blocks.AiBoundFakePlayer;
import com.verr1.controlcraft.unstable.data.AIPersistentData;
import com.verr1.controlcraft.unstable.data.schematic.AISchematic;
import com.verr1.controlcraft.unstable.data.schematic.SchematicKey;
import com.verr1.controlcraft.unstable.management.v1.AISpawnResult;
import com.verr1.controlcraft.unstable.management.v1.RepairResult;
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
import net.minecraft.world.level.ChunkPos;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.saveddata.SavedData;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Quaterniond;
import org.joml.Quaterniondc;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.ships.LoadedServerShip;
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.core.impl.game.ShipTeleportDataImpl;
import org.valkyrienskies.core.internal.world.VsiServerShipWorld;
import org.valkyrienskies.mod.api.ValkyrienSkies;
import org.valkyrienskies.mod.common.BlockStateInfo;
import org.valkyrienskies.mod.common.VSGameUtilsKt;
import org.valkyrienskies.mod.common.assembly.ShipAssembler;

import java.util.*;
import java.util.concurrent.atomic.AtomicReference;

import static org.valkyrienskies.mod.api.ValkyrienSkies.toMinecraft;

public class AIPool extends SavedData {

    private static final Serializer<Map<Long, AIPersistentData>> PERSISTENT = SerializeUtils.ofMap(
        SerializeUtils.LONG,
        AIPersistentData.SER
    );
    public static final String DATA_NAME = ControlCraft.MODID + "_ai_pool";
    public static Vector3d SIMPLE_YARD_POSITION = new Vector3d(0, -64, 0);
    public static Vector3d SIMPLE_CREATE_POSITION = new Vector3d(0, 96, 0);
    public static Vector3d SIMPLE_PROCESS_POSITION = new Vector3d(0, -96, 0); // TODO change it later
    public static int SIMPLE_ARRANGE_SPACING = 3;


    private final Map<Long, AIPersistentData> persistent = new HashMap<>();
    private final Map<Long, AIStatus> statuses = new HashMap<>();

    private final AICoroutineWorker worker = new AICoroutineWorker();


    private final LazyTicker lazyValidator = new LazyTicker(100, this::validate);
    private final LazyTicker lazyYardGuard = new LazyTicker(60, this::ensureStatic);
    private final LazyTicker lazyAttTicker = new LazyTicker(10, this::tickAttachment);
    private final LazyTicker lazyFreeCacher = new LazyTicker(100, this::tickFreeAiCountCache);

    private boolean initialized = false;
    private int cachedFreeAi = 0;


    public @NotNull String dimensionOf(long id){
        return getShipOf(id).map(ServerShip::getChunkClaimDimension).orElse("null");
    }

    public Optional<ServerLevel> getLevelOf(long id){
        return Optional.ofNullable(VSGameUtilsKt.getLevelFromDimensionId(server(), dimensionOf(id)));
    }

    private MinecraftServer server(){
        return Objects.requireNonNull(ControlCraftServer.INSTANCE);
    }

    private VsiServerShipWorld vsWorld(){
        return Objects.requireNonNull(vsWorldNullable());
    }

    private @Nullable VsiServerShipWorld vsWorldNullable(){
        return (VsiServerShipWorld) ValkyrienSkies.getShipWorld(server());
    }

    public @NotNull List<LoadedServerShip> getAllShips(){
        return vsWorld().getLoadedShips().stream().toList();
    }

    public Optional<LoadedServerShip> getShipOf(long id){
        return Optional.ofNullable(vsWorld().getLoadedShips().getById(id));
    }

    public Optional<ServerShip> getPersistentShipOf(long id){
        return Optional.ofNullable(vsWorld().getAllShips().getById(id));
    }

    private Vector3dc yardv(){
        return SIMPLE_YARD_POSITION;
    }

    private ChunkPos yardc(){
        Vector3dc yardv = yardv();
        return new ChunkPos(BlockPos.containing(toMinecraft(yardv)));
    }

    private static ChunkPos toChunkPos(Vector3dc v){
        return new ChunkPos(BlockPos.containing(toMinecraft(v)));
    }

    public AIBlockNetwork getNetworkOf(long id){
        return networkOf(id).orElse(null);
    }

    public Optional<AIPersistentData> getDataOf(long id){
        return Optional.ofNullable(persistent.get(id));
    }

    public Optional<LoadedServerShip> getShipAt(WorldBlockPos pos){
        return Optional
            .ofNullable(VSGameUtilsKt.getLoadedShipManagingPos(pos.level(server()), pos.pos()));
    }

    public static ShipTeleportDataImpl withPosition(Vector3dc newPosition){
        return new ShipTeleportDataImpl(newPosition, new Quaterniond(), new Vector3d(), new Vector3d(), null, null, null);
    }

    public List<Long> listAvailableAI(){
        return statuses.entrySet().stream().filter(e -> e.getValue() == AIStatus.IN_POOL).map(Map.Entry::getKey).toList();
    }

    public List<Long> listFreeAI(){
        return statuses.entrySet().stream().filter(e -> e.getValue() == AIStatus.FREE).map(Map.Entry::getKey).toList();
    }

    public List<Long> listAllAI(){
        return persistent.keySet().stream().toList();
    }

    private Optional<AIBlockNetwork> networkOf(long s){
        return getShipOf(s).map(AIBlockNetwork::getOrCreate);
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
            s.removeAttachment(AIBlockNetwork.class);
            s.setSlug("unmarked_ai_" + randomSequence(5));
        }, () -> logAbsentId(id));
    }

    public boolean isAI(long id){
        Optional<LoadedServerShip> opt = getShipOf(id);
        return opt.filter(ship -> ship.getAttachment(AIBlockNetwork.class) != null).isPresent();
    }

    private void removeData(long i){
        persistent.remove(i);
        statuses.remove(i);
        setDirty();
    }

    public void reset(){
        statuses.clear();
        listAllAI().forEach(this::discard);
        setDirty();
    }

    public void resetAlive(){
        listFreeAI().forEach(this::discard);
        setDirty();
    }

    public void validate(){
        persistent.keySet().stream().filter(i -> getPersistentShipOf(i).isEmpty()).toList().forEach(this::removeData);
    }



    public void discard(long id){
        if(!isAI(id))return;
        VsiServerShipWorld vsWorld = vsWorldNullable();
        if(vsWorld == null)return;
        LoadedServerShip ship = getShipOf(id).orElse(null);
        if(ship == null)return;
        ServerLevel level = getLevelOf(id).orElse(null);
        if(level == null)return;
        networkOf(ship).onDiscard();

        statuses.put(id, AIStatus.ON_DISCARD_PROCESS);

        // avoid holding ship reference across ticks !!
        CoroutineBase teleportFirst = Coroutines.immediate(() -> {
            LoadedServerShip loaded = getShipOf(ship.getId()).orElse(null);
            if(loaded == null)return;
            networkOf(loaded).onPreRestore();
            vsWorld.teleportShip(loaded, withPosition(SIMPLE_PROCESS_POSITION));
            loaded.setStatic(true);
        });
        Either<CoroutineBase, AIRepairErrors> taskOrError = repairAI(id, persistent.get(id).storageSchematic);
        CoroutineBase teleportSecond = Coroutines.immediate(() -> {
            LoadedServerShip loaded = getShipOf(ship.getId()).orElse(null);
            if(loaded == null)return;
            vsWorld.teleportShip(loaded, withPosition(computeYardPosition(id)));
            BlockStateInfo.INSTANCE.remassShip(level, loaded);
            networkOf(loaded).onPostRestore();
            loaded.setStatic(true);
            statuses.put(id, AIStatus.IN_POOL);
        });


        taskOrError.ifLeft(task -> {
            worker.enqueueTask(Coroutines.chained(teleportFirst, task, teleportSecond));
        });
        taskOrError.ifRight(err -> {
            // handling
        });
    }


    public void joinPool(@NotNull SchematicKey defaultSchematic, @NotNull ServerLevel level){
        AISchematic schematic = AIServer.SCHEMATICS_MANAGER.getLoaded(defaultSchematic);
        if(schematic == null){
            logAbsentSchematic(defaultSchematic);
            return;
        }
        ServerShip newAIShip = create1Block(level);
        long id = newAIShip.getId();

        Runnable endTask = () -> {
            LoadedServerShip loaded = getShipOf(id).orElse(null);
            if(loaded == null){
                ControlCraft.LOGGER.error("added ai ship {} is not a loadedShip during endTask, WTF?", id);
                return;
            }
            Vector3dc createdShipCenter = loaded.getInertiaData().getCenterOfMass();
            BlockPos center = BlockPos.containing(toMinecraft(createdShipCenter));
            persistent.put(id, new AIPersistentData(center, defaultSchematic));
            statuses.put(id, AIStatus.FREE);
            markAsAI(id);
            ControlCraftServer.SERVER_EXECUTOR.executeLater(() -> discard(loaded.getId()), 4);
            setDirty();
        };

        var task = new ConditionExecutable
            .builder(endTask)
            .withCondition(() -> getShipOf(id).isPresent())
            .withExpirationTicks(4)
            .withOrElse(() -> {
                ControlCraft.LOGGER.error("added ai ship {} is not a loadedShip in 4 ticks after creation", id);
            })
            .build();

        ControlCraftServer.SERVER_EXECUTOR.execute(task);

    }

    public @NotNull AISpawnResult spawn(SchematicKey type, Vector3dc position, Quaterniondc rotation, boolean immediate){
        return spawn(type, position, rotation, new Vector3d(), new Vector3d(), immediate);
    }



    public @NotNull AISpawnResult spawn(
        SchematicKey type,
        Vector3dc position,
        Quaterniondc rotation,
        Vector3dc velocity,
        Vector3dc omega,
        boolean immediate
    ){
        long id = pollPool().orElse(-1L);
        if(id == -1L)return AISpawnResult.USE_UP;
        LoadedServerShip ship = getShipOf(id).orElse(null);
        if(ship == null)return AISpawnResult.DELETED;
        if(!isAI(id))return AISpawnResult.NOT_AN_AI; // highly unlikely

        statuses.put(id, AIStatus.ON_SPAWN_PROCESS);

        networkOf(ship).onPreRepair();


        CoroutineBase teleport = Coroutines.immediate(() -> {
            ship.setStatic(false);
            networkOf(ship).onPostRepair();
            statuses.put(id, AIStatus.FREE);
            vsWorld().teleportShip(ship, withPose(position, rotation, velocity, omega));
            networkOf(ship).onSpawn();
        });

        var taskOrError = repairAI(id, type);

        AtomicReference<AISpawnResult> atr = new AtomicReference<>(null);
        taskOrError.ifLeft(repairTask -> {
            atr.set(new AISpawnResult(id));
            CoroutineBase task = Coroutines.chained(repairTask, teleport);
            if(immediate){
                task.force();
            }else{
                worker.enqueueTask(task);
            }
        });
        taskOrError.ifRight(err -> {
            // TODO: use err later
            atr.set(new AISpawnResult(-1, AISpawnResult.Status.CAN_NOT_REPAIR, RepairResult.DID_NOT_REPAIR));
        });
        return Objects.requireNonNull(atr.get());
    }



    public static ShipTeleportDataImpl withPose(
        Vector3dc newPosition,
        Quaterniondc newRotation,
        Vector3dc vel,
        Vector3dc omg
    ){
        return new ShipTeleportDataImpl(newPosition, newRotation.normalize(new Quaterniond()), vel, omg, null, null, null);
    }

    private Optional<Long> pollPool(){
        return persistent.keySet().stream().filter(k -> statuses.computeIfAbsent(k, $ -> AIStatus.FREE) == AIStatus.IN_POOL).findAny();
    }

    private AIBlockNetwork networkOf(LoadedServerShip s){
        return AIBlockNetwork.getOrCreate(s);
    }

    public static ServerShip create1Block(ServerLevel level){
        BlockPos createdWorldCenter = BlockPos.containing(toMinecraft(SIMPLE_CREATE_POSITION));
        level.setBlock(createdWorldCenter, Blocks.STONE.defaultBlockState(), 3);

        return ShipAssembler.assembleToShip(level, Set.of(createdWorldCenter), 1.0);
    }

    public Either<CoroutineBase, AIRepairErrors> repairAI(long id, @NotNull SchematicKey overrideKey){
        AIPersistentData data = persistent.get(id);
        if(data == null){
            logAbsentId(id);
            MinecraftUtils.broadcastMessage(Component.literal("Ship with id: " + id + " is not recorded as AI!"));
            return Either.right(AIRepairErrors.ABSENT_AI_DATA);
        }

        AISchematic schematic = AIServer.SCHEMATICS_MANAGER.getLoaded(overrideKey);
        if(schematic == null){
            logAbsentSchematic(overrideKey);
            MinecraftUtils.broadcastMessage(Component.literal("AI with type: " + overrideKey + " has no schematic loaded!"));
            return Either.right(AIRepairErrors.ABSENT_SCHEMATIC);
        }

        ServerLevel level = getLevelOf(id).orElse(null);
        if(level == null){
            ControlCraft.LOGGER.error("Tried to repair AI with id {}, but no level found for the ship", id);
            return Either.right(AIRepairErrors.CAN_NOT_ACCESS_LEVEL);
        }

        LoadedServerShip ship = getShipOf(id).orElse(null);
        if(ship == null){
            ControlCraft.LOGGER.error("Tried to repair AI with id {}, but no ship found.", id);
            return Either.right(AIRepairErrors.CAN_NOT_ACCESS_SHIP);
        }

        BlockPos center = data.center;
        CoroutineBase repair = SchematicCoroutine.make(schematic, level, center, ship);
        CoroutineBase teleport = Coroutines.immediate(() -> {
            Vector3dc p = computeYardPosition(id);
            vsWorld().teleportShip(ship, withPosition(p));
            networkOf(ship).onPostRepair();
        });

        return Either.left(Coroutines.chained(repair, teleport));
    }

    private int computeYardIndex(long id){
        return Math.toIntExact(persistent.keySet().stream().filter(k -> id > k).count());
    }

    private Vector3dc computeYardPosition(long id){
        int index = computeYardIndex(id);
        int x = index % 8;
        int y = index / 8;
        return new Vector3d(SIMPLE_YARD_POSITION).add(x * SIMPLE_ARRANGE_SPACING, 0, y * SIMPLE_ARRANGE_SPACING);
    }

    public void setYardPosition(double x, double y, double z){
        moveYard(SIMPLE_YARD_POSITION, new Vector3d(x, y, z));
        SIMPLE_YARD_POSITION.set(x, y, z);
        SIMPLE_PROCESS_POSITION.set(x, y - 24, z);
        SIMPLE_CREATE_POSITION.set(x, 96, z);
        setDirty();
    }

    private void moveYard(Vector3dc yardvo, Vector3dc yardvn){
        ChunkPos o = toChunkPos(yardvo);
        ChunkPos n = toChunkPos(yardvn);
        ControlCraftServer.OVERWORLD.getChunkSource().removeRegionTicket(ChunkLoader.CHUNK_LOAD_TICKET, o, 3, o.toLong(), false);
        ControlCraftServer.OVERWORLD.getChunkSource().addRegionTicket(ChunkLoader.CHUNK_LOAD_TICKET, n, 3, n.toLong(), false);
    }

    public void tick(){
        if(!initialized){
            initialized = true;
            initialize();
        }
        worker.run();
        lazyValidator.tick();
        lazyYardGuard.tick();
        lazyAttTicker.tick();
        lazyFreeCacher.tick();
    }

    private void initialize(){
        moveYard(SIMPLE_YARD_POSITION, SIMPLE_YARD_POSITION);
    }

    public void close(){
        // shall we ?
        worker.force();
    }

    public int getFreeAICachedCount(){
        return cachedFreeAi;
    }

    public void ensureStatic(){
        listAvailableAI().forEach(i -> getShipOf(i).ifPresent(s -> {
            s.setStatic(true);
            Vector3dc p = computeYardPosition(i);
            vsWorld().teleportShip(s, withPosition(p));
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

    public static String randomSequence(int length){
        StringBuilder sb = new StringBuilder(length);
        Random random = new Random();
        for (int i = 0; i < length; i++) {
            sb.append((char) ('a' + random.nextInt(26))); // 'a' to 'z'
        }
        return sb.toString();
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
            .withCompound("yardc", SerializeUtils.VECTOR3D.serialize(SIMPLE_CREATE_POSITION))
            .withCompound("yardp", SerializeUtils.VECTOR3D.serialize(SIMPLE_PROCESS_POSITION))
            .build();
    }

    public void deserialize(CompoundTag tag){
        persistent.clear();
        persistent.putAll(PERSISTENT.deserialize(tag.getCompound("database")));
        SIMPLE_YARD_POSITION = SerializeUtils.VECTOR3D.deserializeOrElse(tag.getCompound("yard"), SIMPLE_YARD_POSITION);
        SIMPLE_CREATE_POSITION = SerializeUtils.VECTOR3D.deserializeOrElse(tag.getCompound("yardc"), SIMPLE_CREATE_POSITION);
        SIMPLE_PROCESS_POSITION = SerializeUtils.VECTOR3D.deserializeOrElse(tag.getCompound("yardp"), SIMPLE_PROCESS_POSITION);
    }

    private static AIPool load(@NotNull CompoundTag tag) {
        AIPool saved = new AIPool();
        saved.deserialize(tag.getCompound("ai_pool"));
        return saved;
    }


    public static AIPool load(MinecraftServer server){
        return server.overworld().getDataStorage().computeIfAbsent(AIPool::load, AIPool::new, DATA_NAME);
    }

    public void onServerStarted(){
        var task = new ConditionExecutable.builder(this::reset)
                .withCondition(() -> vsWorldNullable() != null)
                .withOrElse(() -> {
                    MinecraftUtils.broadcastMessage("Vs world Failed to load after 400 ticks");
                    ControlCraft.LOGGER.error("VS World Failed To Load");
                })
                .withExpirationTicks(400)
                .build();

        ControlCraftServer.SERVER_EXECUTOR.execute(task);
    }

    public boolean isInPool(long ownerId) {
        return statuses.computeIfAbsent(ownerId, k -> AIStatus.FREE) == AIStatus.IN_POOL;
    }

    enum AIStatus{
        FREE,
        IN_POOL,
        ON_DISCARD_PROCESS,
        ON_SPAWN_PROCESS
    }

    public enum AIRepairErrors{
        ABSENT_SCHEMATIC,
        ABSENT_AI_DATA,
        CAN_NOT_ACCESS_LEVEL,
        CAN_NOT_ACCESS_SHIP
    }

    private static class AICoroutineWorker{
        Queue<CoroutineBase> coroutines = new ArrayDeque<>();

        void run(){
            if(coroutines.isEmpty())return;
            CoroutineBase current = Objects.requireNonNull(coroutines.peek());
            if(current.closed()){
                coroutines.poll();
            }

            current.run(current.suggestedBatch());
        }

        void force(){
            while(!coroutines.isEmpty()){
                coroutines.poll().force();
            }
        }

        void enqueueTask(@NotNull CoroutineBase task){
            coroutines.add(task);
        }

    }
}
