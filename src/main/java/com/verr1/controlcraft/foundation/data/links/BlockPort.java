package com.verr1.controlcraft.foundation.data.links;

import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import org.jetbrains.annotations.NotNull;

public record BlockPort(@NotNull WorldBlockPos pos, String portName) {
    public static final BlockPort EMPTY = new BlockPort(WorldBlockPos.NULL, "");

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!(obj instanceof BlockPort other)) return false;
        return pos.equals(other.pos) && portName.equals(other.portName);
    }

    @Override
    public String toString() {
        return "[" + portName + " | " + pos.pos().toShortString() + "]";
    }

    public BlockPort offset(BlockPos offsetPos){
        return new BlockPort(new WorldBlockPos(pos.dimensionID(), pos.pos().offset(offsetPos)), portName);
    }

    @Override
    public int hashCode() {
        return pos.hashCode() ^ portName().hashCode();
    }

    public CompoundTag serialize(){
        return CompoundTagBuilder.create()
                .withCompound("pos", pos.serialize())
                .withCompound("port_name", SerializeUtils.STRING.serialize(portName))
                .build();
    }

    public static BlockPort deserialize(CompoundTag tag){
        return new BlockPort(
                WorldBlockPos.deserialize(tag.getCompound("pos")),
                SerializeUtils.STRING.deserialize(tag.getCompound("port_name"))
        );
    }



}
