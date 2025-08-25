package com.verr1.controlcraft.unstable.util;

import com.verr1.controlcraft.utils.Serializer;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.state.BlockState;
import net.spaceeye.valkyrien_ship_schematics.containers.v1.BlockItem;
import net.spaceeye.valkyrien_ship_schematics.containers.v1.BlockPaletteHashMapV1;
import net.spaceeye.valkyrien_ship_schematics.containers.v1.ChunkyBlockData;

import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;

public class SchematicSerializeUtil {


    public static CompoundTag serializePalette(BlockPaletteHashMapV1 palette){
        CompoundTag tag = new CompoundTag();
        int size = palette.getPaletteSize();
        tag.putInt("size", size);
        for(int i = 0; i < size; i++){
            BlockState state = Objects.requireNonNull(palette.fromId(i));

            tag.put(
                    String.valueOf(i),
                    BlockState.CODEC
                            .encodeStart(NbtOps.INSTANCE, state)
                            .resultOrPartial(error -> System.err.println("Failed to serialize BlockState: " + error))
                            .orElse(new CompoundTag())
            );
        }
        return tag;
    }

    public static BlockPaletteHashMapV1 deserializePalette(CompoundTag tag) throws NoSuchElementException {
        BlockPaletteHashMapV1 palette = new BlockPaletteHashMapV1();
        int size = tag.getInt("size");
        for(int i = 0; i < size; i++){
            String key = String.valueOf(i);
            if(tag.contains(key)){
                CompoundTag stateTag = tag.getCompound(key);
                BlockState state = BlockState.CODEC
                        .parse(NbtOps.INSTANCE, stateTag)
                        .resultOrPartial(error -> {
                            System.err.println("Failed to deserialize BlockState: " + error);
                        })
                        .orElseThrow();
                palette.toId(state);
            }
        }
        return palette;
    }

    public static CompoundTag serializeChunkyBlockData(ChunkyBlockData<BlockItem> blockData){
        CompoundTag tag = new CompoundTag();
        AtomicInteger size = new AtomicInteger(0);
        blockData.forEach((x, y, z, blockItem) -> {
            BlockPos pos = new BlockPos(x, y, z);
            CompoundTag blockTag = new CompoundTag();
            blockTag.putInt("paletteId", blockItem.getPaletteId());
            blockTag.putInt("extraDataId", blockItem.getExtraDataId());
            blockTag.putLong("pos", pos.asLong());
            tag.put(size.getAndIncrement() + "", blockTag);
            return null;
        });
        tag.putInt("size", size.get());
        return tag;
    }

    public static ChunkyBlockData<BlockItem> deserializeChunkyBlockData(CompoundTag tag){
        ChunkyBlockData<BlockItem> blockData = new ChunkyBlockData<>();
        int size = tag.getInt("size");
        for(int i = 0; i < size; i++){
            String key = String.valueOf(i);
            if(tag.contains(key)){
                CompoundTag blockTag = tag.getCompound(key);
                int paletteId = blockTag.getInt("paletteId");
                int extraDataId = blockTag.getInt("extraDataId");
                long posLong = blockTag.getLong("pos");
                BlockPos pos = BlockPos.of(posLong);
                BlockItem item = new BlockItem(paletteId, extraDataId);
                blockData.add(pos.getX(), pos.getY(), pos.getZ(), item);
            }
        }
        return blockData;
    }

}
