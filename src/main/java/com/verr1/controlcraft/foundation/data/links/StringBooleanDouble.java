package com.verr1.controlcraft.foundation.data.links;

import net.minecraft.nbt.CompoundTag;

public record StringBooleanDouble (String name, boolean enabled, double value){

    public CompoundTag serialize(){
        CompoundTag tag = new CompoundTag();
        tag.putString("name", name);
        tag.putBoolean("enabled", enabled);
        tag.putDouble("value", value);
        return tag;
    }

    public static StringBooleanDouble deserialize(CompoundTag tag){
        return new StringBooleanDouble(
                tag.getString("name"),
                tag.getBoolean("enabled"),
                tag.getDouble("value")
        );
    }

}
