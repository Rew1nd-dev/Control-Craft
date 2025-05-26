package com.verr1.controlcraft.foundation.data;

import net.minecraft.nbt.CompoundTag;
import org.jetbrains.annotations.NotNull;

public record PeripheralKey_(String Name, long Protocol) {
    public static PeripheralKey_ NULL = new PeripheralKey_("null", 0);
    @Override
    public int hashCode() {
        return Long.hashCode(Protocol);
    }

    public CompoundTag serialize(){
        CompoundTag tag = new CompoundTag();
        tag.putString("name", Name);
        tag.putLong("protocol", Protocol);
        return tag;
    }

    public static @NotNull PeripheralKey_ deserialize(CompoundTag tag){
        if (tag == null) return NULL;
        if(!(tag.contains("name") && tag.contains("protocol")))return NULL;
        return new PeripheralKey_(tag.getString("name"), tag.getLong("protocol"));
    }
}
