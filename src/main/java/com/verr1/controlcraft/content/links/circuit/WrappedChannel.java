package com.verr1.controlcraft.content.links.circuit;

import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;

import java.util.ArrayList;
import java.util.List;

public class WrappedChannel {

    private static final Serializer<List<IoData>> SERIALIZER =
            SerializeUtils.ofList(SerializeUtils.of(
                    IoData::serialize,
                    IoData::deserialize
            ));

    public final BlockPos pos;
    public final List<IoData> ioDatas = new ArrayList<>();
    private final CompoundTag inventoryTag = new CompoundTag();

    public void overrideData(List<CircuitBlockEntity.WirelessIO> ios){
        ioDatas.clear();
        ios.forEach(io -> {
            ioDatas.add(new IoData(
                    io.minMax.get(true),
                    io.minMax.get(false),
                    io.ioName,
                    io.enabled,
                    io.isInput
            ));
        });
    }

    public int size(){
        return ioDatas.size();
    }

    public void write(FriendlyByteBuf buffer){
        buffer.writeNbt(SERIALIZER.serialize(ioDatas));
        buffer.writeBlockPos(pos);
    }

    public CompoundTag inventoryTag() {
        return inventoryTag;
    }

    public WrappedChannel(FriendlyByteBuf buf){
        ioDatas.addAll(SERIALIZER.deserialize(buf.readNbt()));
        this.pos = buf.readBlockPos();
    }

    public WrappedChannel(BlockPos pos){
        this.pos = pos;
    }

    public void serialize(CompoundTag invNbt){
        inventoryTag.put("items", invNbt);
    }

    public CompoundTag saveToTag(){
        CompoundTag tag = new CompoundTag();
        tag.put("inv", inventoryTag.getCompound("items"));
        return tag;
    }

    public void loadFromTag(CompoundTag tag){
        serialize(tag.getCompound("inv"));
    }

}
