package com.verr1.controlcraft.foundation.data.links;

import net.minecraft.nbt.CompoundTag;

public record CircuitPortStatus(String portName, double value, boolean isInput, boolean enabled) {

    // generate serialize and deserialize

    public CompoundTag serialize() {
        CompoundTag tag = new CompoundTag();
        tag.putString("portName", portName);
        tag.putDouble("value", value);
        tag.putBoolean("isInput", isInput);
        tag.putBoolean("enabled", enabled);
        return tag;
    }

    public static CircuitPortStatus deserialize(CompoundTag tag) {
        return new CircuitPortStatus(
                tag.getString("portName"),
                tag.getDouble("value"),
                tag.getBoolean("isInput"),
                tag.getBoolean("enabled")
        );
    }

}
