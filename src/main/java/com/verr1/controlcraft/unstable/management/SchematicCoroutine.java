package com.verr1.controlcraft.unstable.management;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.registry.AIBlocks;
import com.verr1.controlcraft.unstable.AIServer;
import com.verr1.controlcraft.unstable.data.schematic.AISchematic;
import com.verr1.controlcraft.utils.MathUtils;
import kotlin.Pair;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.NotNull;
import org.joml.Vector3i;
import org.joml.primitives.AABBi;
import org.joml.primitives.AABBic;
import org.valkyrienskies.core.api.ships.LoadedServerShip;
import org.valkyrienskies.core.api.ships.Ship;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static net.minecraft.world.level.block.Block.UPDATE_CLIENTS;
import static net.minecraft.world.level.block.Block.UPDATE_SUPPRESS_DROPS;

public class SchematicCoroutine {

    final AISchematic schematic;
    final ServerLevel blockPlacer;
    final BlockPos centerPosition;
    final LoadedServerShip ship;

    static final int UPDATE_BIT = UPDATE_CLIENTS | UPDATE_SUPPRESS_DROPS; //32;//

    public SchematicCoroutine(
        @NotNull AISchematic schematic,
        @NotNull ServerLevel blockPlacer,
        @NotNull BlockPos centerPosition,
        @NotNull LoadedServerShip ship
    ) {
        this.schematic = schematic;
        this.blockPlacer = blockPlacer;
        this.centerPosition = centerPosition;
        this.ship = ship;
    }

    public LoadedServerShip ship() {
        return ship;
    }

    public boolean isValid(){
        return Optional.ofNullable(ship()).map(Ship::getShipAABB).isPresent();
    }

    public WorldBlockPos center(){
        return WorldBlockPos.of(blockPlacer, centerPosition);
    }

    public BlockPos currentCenter() {
        return centerPosition;
    }

    public ServerLevel currentLevel() {
        return blockPlacer;
    }

    public AISchematic schematic() {
        return schematic;
    }

    public BlockPos toWorld(BlockPos schematicOffsetPosition){
        return schematicOffsetPosition.offset(centerPosition);
    }

    public BlockPos toOffset(BlockPos schematicOffsetPosition){
        return schematicOffsetPosition.offset(-centerPosition.getX(), -centerPosition.getY(), -centerPosition.getZ());
    }

    public BlockPos.MutableBlockPos toWorld(int x, int y, int z, BlockPos.MutableBlockPos dest){
        return dest.set(x + centerPosition.getX(), y + centerPosition.getY(), z + centerPosition.getZ());
    }

    public BlockPos.MutableBlockPos toOffset(int x, int y, int z, BlockPos.MutableBlockPos dest){
        return dest.set(x - centerPosition.getX(), y - centerPosition.getY(), z - centerPosition.getZ());
    }

    protected boolean validCenter(){
        return AIServer.MANAGER.getShipAt(center()).filter(s -> s.getId() == ship().getId()).isPresent();
    }

    protected SerialRoutine create(){
        return Coroutines.chained(new ClearanceRoutine(), new PlacementRoutine());
    }

    public static SerialRoutine make(
        @NotNull AISchematic schematic,
        @NotNull ServerLevel blockPlacer,
        @NotNull BlockPos centerPosition,
        @NotNull LoadedServerShip ship
    ){
        return new SchematicCoroutine(schematic, blockPlacer, centerPosition, ship).create();
    }


    public class ClearanceRoutine extends CoroutineBase{

        protected final AABBi currentOffsetBounds = new AABBi();

        int x = 0;
        int y = 0;
        int z = 0;
        int cycle = 0;

        @Override
        protected boolean canResume() {
            return isValid();
        }

        @Override
        public RoutineStatus run(int steps) {
            ensureCenterNotCleared();
            int repaired = 0;
            BlockPos.MutableBlockPos worldPosition = new BlockPos.MutableBlockPos();
            // Outliner.getInstance().chaseAABB(-1, new AABB(currentOffsetBounds.minX, currentOffsetBounds.minY, currentOffsetBounds.minZ, currentOffsetBounds.maxX, currentOffsetBounds.maxY, currentOffsetBounds.maxZ).move(centerPosition)).colored(Color.SPRING_GREEN).lineWidth(1 / 16f);
            for(; x <= currentOffsetBounds.maxX; x++){
                for(; y <= currentOffsetBounds.maxY; y++){
                    for(; z <= currentOffsetBounds.maxZ; z++){
                        if(repaired >= steps){
                            return RoutineStatus.FINISHED_STEP;
                        }

                        worldPosition = toWorld(x, y, z, worldPosition); //
                        if(worldPosition.equals(centerPosition) || blockPlacer.getBlockState(worldPosition).isAir())continue; //
                        blockPlacer.setBlock(worldPosition, Blocks.AIR.defaultBlockState(), UPDATE_BIT); //
                        // Outliner.getInstance().chaseAABB(repaired, AABB.ofSize(worldPosition.getCenter(), 1, 1, 1));
                        repaired++;

                    }
                    z = currentOffsetBounds.minZ;
                }
                y = currentOffsetBounds.minY;
                z = currentOffsetBounds.minZ;
            }
            cycle++;
            init();
            // if not cleared (like something may add new blocks when broken, e.g. cannon mount), try again
            // and avoid broken aabb for unknown reason
            if(singleBlockBound() || cycle > 1){
                close();
                return RoutineStatus.FINISHED_ALL;
            }

            return RoutineStatus.FINISHED_STEP;
        }

        void ensureCenterNotCleared(){
            if(!blockPlacer.getBlockState(centerPosition).is(AIBlocks.SCHEMATIC.get())){
                blockPlacer.setBlock(centerPosition, AIBlocks.SCHEMATIC.getDefaultState(), UPDATE_BIT);
            }
        }

        boolean singleBlockBound(){
            return
                currentOffsetBounds.maxX == currentOffsetBounds.minX + 1 &&
                currentOffsetBounds.maxY == currentOffsetBounds.minY + 1 &&
                currentOffsetBounds.maxZ == currentOffsetBounds.minZ + 1;
        }

        @Override
        protected void init() {
            updateBounds();
            x = currentOffsetBounds.minX;
            y = currentOffsetBounds.minY;
            z = currentOffsetBounds.minZ;
        }

        protected void updateBounds() {
            LoadedServerShip ship = ship();
            if (ship == null) return;
            AABBic aabb = ship.getShipAABB();
            if (aabb == null) return;
            AABBic offsetAABB = MathUtils.offset(aabb, new Vector3i(-currentCenter().getX(), -currentCenter().getY(), -currentCenter().getZ()));

            currentOffsetBounds.set(offsetAABB);
            MathUtils.inflate(currentOffsetBounds, 2, 2, 2);
        }

        @Override
        public int suggestedBatch() {
            return 64;
        }
    }


    public class PlacementRoutine extends CoroutineBase{
        protected final ArrayDeque<Pair<BlockPos, AISchematic.BlockItemResult>> currentAccessQueue = new ArrayDeque<>();

        List<Runnable> deferredLoading = new ArrayList<>();

        @Override
        public RoutineStatus run(int steps) {
            int placed = 0;
            while (!currentAccessQueue.isEmpty()){
                if(placed >= steps)return RoutineStatus.FINISHED_STEP;
                var item = currentAccessQueue.poll();
                BlockPos offsetPosition = item.getFirst();
                AISchematic.BlockItemResult bir = item.getSecond();

                BlockState originalState = bir.state();
                CompoundTag nullableTag = bir.tag();

                BlockPos worldPosition = toWorld(offsetPosition);

                blockPlacer.setBlock(worldPosition, originalState, UPDATE_BIT);
                BlockEntity be = blockPlacer.getBlockEntity(worldPosition);

                if(be != null && nullableTag != null){
                    deferredLoading.add(tryLoad(blockPlacer, worldPosition, nullableTag));
                }

                placed++;
            }

            deferredLoading.forEach(Runnable::run);
            closed = true;
            return RoutineStatus.FINISHED_ALL;
        }

        @Override
        protected boolean canResume() {
            return isValid();
        }

        static Runnable tryLoad(ServerLevel level, BlockPos worldPosition, CompoundTag tag){
            return () -> {
                  try{
                      BlockEntity be = level.getBlockEntity(worldPosition);
                      if(be == null)return;
                      be.load(tag);
                  }catch (Exception e){
                      ControlCraft.LOGGER.error("Error invode be load during Placement Routine: at {}, tag {}, e: {}",
                          worldPosition,
                          tag,
                          e.getMessage()
                      );
                  }
            };
        }


        @Override
        protected void init() {
            updateQueue();
        }

        void updateQueue() {
            currentAccessQueue.addAll(schematic().blocksWithFilter($ -> true));
        }

        void enqueue(BlockPos worldPosition) {
            AISchematic.BlockItemResult bir = schematic().get(toOffset(worldPosition));
            currentAccessQueue.addLast(new Pair<>(toOffset(worldPosition), bir));
        }


        @Override
        public int suggestedBatch() {
            return 64;
        }
    }


}
