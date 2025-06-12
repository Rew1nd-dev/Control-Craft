package com.verr1.controlcraft.content.links.circuit;

import net.minecraft.nbt.CompoundTag;

public record IoData(double min, double max, String ioName, boolean enabled, boolean isInput) {

    public CompoundTag serialize(){
        CompoundTag tag = new CompoundTag();
        tag.putDouble("min", min);
        tag.putDouble("max", max);
        tag.putString("ioName", ioName);
        tag.putBoolean("enabled", enabled);
        tag.putBoolean("isInput", isInput);
        return tag;
    }

    public static IoData deserialize(CompoundTag tag){
        return new IoData(
                tag.getDouble("min"),
                tag.getDouble("max"),
                tag.getString("ioName"),
                tag.getBoolean("enabled"),
                tag.getBoolean("isInput")
        );
    }
}
