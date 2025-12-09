package com.verr1.controlcraft.foundation.cimulink.game.circuit;

import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.nbt.CompoundTag;
import org.jetbrains.annotations.NotNull;

public record IoNbt(boolean isInput, String ioName, String componentName, String portName) {

    public CompoundTag serialize(){
        return CompoundTagBuilder.create()
                .withCompound("isInput", SerializeUtils.BOOLEAN.serialize(isInput))
                .withCompound("ioName", SerializeUtils.STRING.serialize(ioName))
                .withCompound("componentName", SerializeUtils.STRING.serialize(componentName))
                .withCompound("portName", SerializeUtils.STRING.serialize(portName))
                .build();
    }

    public static IoNbt deserialize(CompoundTag tag){
        return new IoNbt(
                SerializeUtils.BOOLEAN.deserialize(tag.getCompound("isInput")),
                SerializeUtils.STRING.deserialize(tag.getCompound("ioName")),
                SerializeUtils.STRING.deserialize(tag.getCompound("componentName")),
                SerializeUtils.STRING.deserialize(tag.getCompound("portName"))
        );
    }

    @Override
    public @NotNull String toString() {
        String in_out = isInput ? "IN" : "OUT";
        return String.format("[(Global %s.%s) -> (%s.%s)]", in_out, ioName, componentName, portName);
    }
}
