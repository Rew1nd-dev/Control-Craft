package com.verr1.controlcraft.content.links.circuit;

import net.minecraft.nbt.CompoundTag;

public record IoSettings (double min, double max, boolean enabled){

    public CompoundTag serialize(){
        CompoundTag tag = new CompoundTag();
        tag.putDouble("min", min);
        tag.putDouble("max", max);
        tag.putBoolean("enabled", enabled);
        return tag;
    }

    public static IoSettings deserialize(CompoundTag tag){
        return new IoSettings(
                tag.getDouble("min"),
                tag.getDouble("max"),
                tag.getBoolean("enabled")
        );
    }

}
