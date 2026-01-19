package com.verr1.controlcraft.unstable.data.schematic;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.unstable.AIServer;
import com.verr1.controlcraft.unstable.ai.api.IReplaceBlock;
import com.verr1.controlcraft.unstable.util.SchematicSerializeUtil;
import com.verr1.controlcraft.utils.*;
import kotlin.Pair;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.ChunkPos;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.chunk.LevelChunk;
import net.minecraft.world.phys.AABB;
import net.spaceeye.valkyrien_ship_schematics.containers.v1.BlockItem;
import net.spaceeye.valkyrien_ship_schematics.containers.v1.BlockPaletteHashMapV1;
import net.spaceeye.valkyrien_ship_schematics.containers.v1.ChunkyBlockData;
import net.spaceeye.valkyrien_ship_schematics.interfaces.ICopyableBlock;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Vector3d;
import org.joml.Vector3i;
import org.joml.primitives.AABBi;
import org.joml.primitives.AABBic;
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.core.api.ships.Ship;
import org.valkyrienskies.mod.api.ValkyrienSkies;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Predicate;

import static org.valkyrienskies.mod.api.ValkyrienSkies.toJOML;


public class AISchematic {
    private static final Serializer<BlockPaletteHashMapV1> PALETTE = SerializeUtils.of(
            SchematicSerializeUtil::serializePalette,
            SchematicSerializeUtil::deserializePalette
    );

    private static final Serializer<ChunkyBlockData<BlockItem>> BLOCK_DATA = SerializeUtils.of(
            SchematicSerializeUtil::serializeChunkyBlockData,
            SchematicSerializeUtil::deserializeChunkyBlockData
    );

    private static final Serializer<List<CompoundTag>> BE_TAGS = SerializeUtils.ofList(SerializeUtils.UNIT);
    private static final Serializer<Pair<Integer, Integer>> Y_BOUND = SerializeUtils.ofPair(SerializeUtils.INT);


    private final BlockPaletteHashMapV1 offsetPalette;
    private final List<CompoundTag> savedBeTags;
    private final ChunkyBlockData<BlockItem> blockData;
    private final Pair<Integer, Integer> yBound;
    private final long oldShipId;
    private final BlockPos oldShipChunkCenter;
    private final double mass;

    AISchematic(
            @NotNull BlockPaletteHashMapV1 offsetPalette,
            @NotNull List<CompoundTag> savedBeTags,
            @NotNull ChunkyBlockData<BlockItem> blockData,
            @NotNull Pair<Integer, Integer> yBound,
            long oldShipId,
            @NotNull BlockPos oldShipChunkCenter,
            double mass
    ) {
        this.offsetPalette = offsetPalette;
        this.savedBeTags = savedBeTags;
        this.blockData = blockData;
        this.yBound = yBound;
        this.oldShipId = oldShipId;
        this.oldShipChunkCenter = oldShipChunkCenter;
        this.mass = mass;
    }

    public double mass(){
        return mass;
    }

    public int size(){
        AtomicInteger sizeOfBytes = new AtomicInteger(0);
        savedBeTags.forEach(
                tag -> {
                    sizeOfBytes.addAndGet(tag.sizeInBytes());
                });
        return sizeOfBytes.get();
    }

    public static @NotNull AISchematic create(ServerShip ship, BlockPos center, ServerLevel level){
        BlockPaletteHashMapV1 palette = new BlockPaletteHashMapV1();
        List<CompoundTag> savedBeTags = new ArrayList<>();
        ChunkyBlockData<BlockItem> blockData = new ChunkyBlockData<>();

        AABBi shipBound = createBound(ship);
        Pair<Integer, Integer> yBound = new Pair<>(
                shipBound.minY(),
                shipBound.maxY()
        );
        List<ChunkPos> allChunkPos = createToIterate(shipBound);
        allChunkPos.forEach(
                chunkPos -> {
                    LevelChunk chunkToSave = level.getChunk(chunkPos.x, chunkPos.z);
                    for(int ix = 0; ix < 16; ix++){
                        for(int iy = yBound.getFirst(); iy < yBound.getSecond(); iy++){
                            for(int iz = 0; iz < 16; iz++){
                                BlockPos pos = new BlockPos(
                                        chunkPos.getMinBlockX() + ix,
                                        iy,
                                        chunkPos.getMinBlockZ() + iz
                                );
                                BlockState state = chunkToSave.getBlockState(pos);

                                try{
                                    if(state.isAir())continue;

                                    int paletteId = palette.toId(state);

                                    // VManagerMod.LOGGER.info("Saving block at: " + pos + " " + chunkToSave.getBlockState(pos).getBlock().getDescriptionId());

                                    BlockEntity be = chunkToSave.getBlockEntity(pos);

                                    CompoundTag nullableTag = Optional.ofNullable(be).map(BlockEntity::saveWithFullMetadata).orElse(null);

                                    if(state.getBlock() instanceof ICopyableBlock cpy){
                                        nullableTag = cpy.onCopy(
                                                level,
                                                pos,
                                                state,
                                                be,
                                                List.of(ship),
                                                Map.of(ship.getId(), toJOML(center.getCenter()))
                                        );
                                    }

                                    int beTagId = Optional
                                            .ofNullable(nullableTag)
                                            // not very functional oriented, but I don't care
                                            .map(tag -> {

                                                // VManagerMod.LOGGER.info("Saving block entity with size: " + tag.sizeInBytes());

                                                savedBeTags.add(tag);
                                                return savedBeTags.size() - 1;
                                            })
                                            .orElse(-1);

                                    BlockItem item = new BlockItem(paletteId, beTagId);
                                    blockData.add(pos.getX() - center.getX(), pos.getY() - center.getY(), pos.getZ() - center.getZ(), item);
                                }catch (Exception e){
                                    ControlCraft.LOGGER.error("error during snap: state: {}, exception: {}", state, DebugUtils.stackTrace(e));
                                }



                            }}}
                }
        );
        return new AISchematic(
                palette,
                savedBeTags,
                blockData,
                new Pair<>(yBound.getFirst() - center.getY(), yBound.getSecond() - center.getY()),
                ship.getId(),
                center,
                ship.getInertiaData().getMass()
        );
    }

    public void repairAt(BlockPos center, ServerLevel blockPlacer){
        Ship ship = ValkyrienSkies.getShipManagingBlock(blockPlacer, center);
        if(ship == null)return;
        AABBic aabb = AIServer.MANAGER.getShipAt(WorldBlockPos.of(blockPlacer, center)).map(Ship::getShipAABB).orElse(null);
        if(aabb == null)return;
        AABBic offsetAABB = MathUtils.offset(aabb, new Vector3i(-center.getX(), -center.getY(), -center.getZ()));

        Set<ChunkPos> chunkToCheck = new HashSet<>(createToIterate(offsetAABB));

        Pair<Integer, Integer> yBound = new Pair<>(
                Math.min(offsetAABB.minY(), this.yBound.getFirst()),
                Math.max(offsetAABB.maxY(), this.yBound.getSecond()) // ensure we don't go out of bounds
        );

        blockData.getBlocks().forEach(
                (offsetChunkBlockPos, map) -> {
                    ChunkPos offsetChunkPos = new ChunkPos(offsetChunkBlockPos.getX(), offsetChunkBlockPos.getZ()); // that is what defined in spaceeye's code
                    chunkToCheck.add(offsetChunkPos);
                }
        );
        List<Runnable> delayLoading = new ArrayList<>();
        chunkToCheck.forEach(offsetChunkPos -> {
            Map<BlockPos, BlockItem> map = blockData.getBlocks().getOrDefault(new BlockPos(offsetChunkPos.x, 0, offsetChunkPos.z), new HashMap<>());
            repairFullChunk(ship, blockPlacer, offsetChunkPos, map, center, yBound, delayLoading);
        });
        // ControlCraftServer.SERVER_EXECUTOR.executeLater(() -> delayLoading.forEach(Runnable::run), 5);
        delayLoading.forEach(Runnable::run);
    }

    private void repairFullChunk(
            Ship ship,
            ServerLevel blockPlacer,
            ChunkPos offsetChunkPos,
            Map<BlockPos, BlockItem> map,
            BlockPos center,
            Pair<Integer, Integer> yBound,
            List<Runnable> delayLoadings
    ){
        for(int ix = 0; ix < 16; ix++){
            for(int iy = yBound.getFirst(); iy < yBound.getSecond(); iy++){
                for(int iz = 0; iz < 16; iz++) {
                    BlockPos offsetPos = new BlockPos(
                            offsetChunkPos.getMinBlockX() + ix,
                            iy,
                            offsetChunkPos.getMinBlockZ() + iz
                    );

                    BlockPos posKey = new BlockPos(ix, iy, iz);


                    BlockState original = null;
                    try {
                        var saved = map.get(posKey);

                        original = Optional
                                .ofNullable(saved)
                                .map(s -> offsetPalette.fromId(s.getPaletteId()))
                                .orElse(Blocks.AIR.defaultBlockState());



                        int extraId = Optional.ofNullable(saved).map(BlockItem::getExtraDataId).orElse(-1);


                        CompoundTag beTag = Optional.of(extraId).filter(id -> id >= 0 && id < savedBeTags.size())
                                .map(savedBeTags::get)
                                .map(CompoundTag::copy) // we don't want to modify original
                                .orElse(new CompoundTag());


                        BlockPos realPos = offsetPos.offset(center);
                        blockPlacer.setBlock(realPos, Blocks.AIR.defaultBlockState(), 64);
//                        if(!original.isAir()){
//                            ControlCraft.LOGGER.debug("fixing: {} with state: {} and beTag size: {} Bytes", realPos, original, beTag.sizeInBytes());
//                        }
//                        if(original.getBlock() instanceof IReplaceBlock){
//                            blockPlacer.setBlock(realPos, Blocks.AIR.defaultBlockState(), 3); // destroy and replace, in order to clear be
//                        }
                        blockPlacer.setBlock(realPos, original, 3);



                        if(original.getBlock() instanceof ICopyableBlock cpy){
                            Vector3d newChunkCenter = toJOML(center.getCenter());
                            Vector3d oldChunkCenter = toJOML(oldShipChunkCenter.getCenter());
                            cpy.onPaste(
                                    blockPlacer,
                                    realPos,
                                    original,
                                    Map.of(oldShipId, ship.getId()),
                                    Map.of(oldShipId, new Pair<>(oldChunkCenter, newChunkCenter)),
                                    beTag
                            );
                        }


                        Optional.ofNullable(
                                blockPlacer.getBlockEntity(realPos)
                        ).ifPresent(
                                be -> delayLoadings.add(() -> {
                                    be.load(beTag);
                                })
                        );

                    } catch (Exception e) {
                        ControlCraft.LOGGER.error("Exception caught during rewinding: pos: {}, state: {}, exception: {}", offsetPos.toShortString(), original, DebugUtils.stackTrace(e));
                        ControlCraft.LOGGER.error(DebugUtils.stackTrace(e));
                    }


                }}}
    }

    public AABB computeBounds(){
        BlockPos.MutableBlockPos min = new BlockPos.MutableBlockPos();
        BlockPos.MutableBlockPos max = new BlockPos.MutableBlockPos();
        blockData.forEach((x, y, z, item) -> {
            min.set(Math.min(x, min.getX()), Math.min(y, min.getY()), Math.min(z, min.getZ()));
            max.set(Math.max(x, max.getX()), Math.max(y, max.getY()), Math.max(z, max.getZ()));
            return null;
        });
        return new AABB(min.getX(), min.getY(), min.getZ(), max.getX() + 1, max.getY() + 1, max.getZ() + 1);
    }

    public @NotNull BlockItemResult get(BlockPos offsetPosition){
        BlockItem item = getRaw(offsetPosition);
        if(item == null)return new BlockItemResult(Blocks.AIR.defaultBlockState(), null);
        BlockState state = Objects.requireNonNullElseGet(offsetPalette.fromId(item.getPaletteId()), Blocks.AIR::defaultBlockState);
        CompoundTag tag = item.getExtraDataId() == -1 ? null : savedBeTags.get(item.getExtraDataId()).copy();
        return new BlockItemResult(state, tag);
    }

    protected BlockItem getRaw(BlockPos offsetPosition){
        BlockPos chunkMin = new BlockPos((offsetPosition.getX() >> 4) << 4, 0, (offsetPosition.getZ() >> 4) << 4);
        return blockData
            .getBlocks()
            .getOrDefault(
                new BlockPos(offsetPosition.getX() >> 4, 0, offsetPosition.getZ() >> 4),
                new HashMap<>()
            )
            .get(offsetPosition.subtract(chunkMin));
    }

    public List<Pair<BlockPos, BlockItemResult>> blocksWithFilter(Predicate<Pair<BlockPos, BlockState>> filter){
        List<Pair<BlockPos, BlockItemResult>> result = new ArrayList<>();
        blockData.forEach((x, y, z, item) -> {
            BlockState state = offsetPalette.fromId(item.getPaletteId());
            if(state == null){
                state = Blocks.AIR.defaultBlockState();
            }

            BlockPos pos = new BlockPos(x, y, z);
            Pair<BlockPos, BlockState> pair = new Pair<>(pos, state);
            if(filter.test(pair)){
                CompoundTag tag = item.getExtraDataId() == -1 ? null : savedBeTags.get(item.getExtraDataId()).copy();
                result.add(new Pair<>(pos, new BlockItemResult(state, tag)));
            }
            return null;
        });
        return result;
    }

    public record BlockItemResult(@NotNull BlockState state, @Nullable CompoundTag tag){};



    public static List<ChunkPos> createToIterate(@NotNull AABBic shipBound){
        BlockPos min = new BlockPos(shipBound.minX(), shipBound.minY(), shipBound.minZ());
        BlockPos max = new BlockPos(shipBound.maxX(), shipBound.maxY(), shipBound.maxZ());
        List<ChunkPos> allChunkPos = new ArrayList<>();
        for (int x = min.getX() - 16; x <= max.getX() + 16; x += 16) {
            for (int z = min.getZ() - 16; z <= max.getZ() + 16; z += 16) {
                allChunkPos.add(new ChunkPos(x >> 4, z >> 4));
            }
        }
        return allChunkPos;
    }

    public static @NotNull AABBi createBound(ServerShip ship){
        AABBic ic = Optional.ofNullable(ship.getShipAABB()).map(AABBi::new).orElse(new AABBi());
        return new AABBi(
                ic.minX() - 1,
                ic.minY() - 1,
                ic.minZ() - 1,
                ic.maxX() + 1,
                ic.maxY() + 1,
                ic.maxZ() + 1
        );
    }


    public CompoundTag serialize(){
        return CompoundTagBuilder.create()
                .withCompound("offset_palette", PALETTE.serialize(offsetPalette))
                .withCompound("saved_be_tags", BE_TAGS.serialize(savedBeTags))
                .withCompound("block_data", BLOCK_DATA.serialize(blockData))
                .withCompound("y_bound", Y_BOUND.serialize(yBound))
                .withCompound("mass", SerializeUtils.DOUBLE.serialize(mass))
                .withCompound("old", SerializeUtils.LONG.serialize(oldShipId))
                .withCompound("oldChunkCenter", SerializeUtils.BLOCK_POS.serialize(oldShipChunkCenter))
                .build();

    }

    public static AISchematic deserialize(CompoundTag tag){
        return new AISchematic(
                PALETTE.deserialize(tag.getCompound("offset_palette")),
                BE_TAGS.deserialize(tag.getCompound("saved_be_tags")),
                BLOCK_DATA.deserialize(tag.getCompound("block_data")),
                Y_BOUND.deserialize(tag.getCompound("y_bound")),
                SerializeUtils.LONG.deserialize(tag.getCompound("old")),
                SerializeUtils.BLOCK_POS.deserialize(tag.getCompound("oldChunkCenter")),
                SerializeUtils.DOUBLE.deserialize(tag.getCompound("mass"))
        );
    }

}
