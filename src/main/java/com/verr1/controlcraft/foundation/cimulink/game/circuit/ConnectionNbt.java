package com.verr1.controlcraft.foundation.cimulink.game.circuit;

import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.nbt.CompoundTag;

public record ConnectionNbt(
        String outputName,
        String outputPortName,
        String inputName,
        String inputPortName
) {

    public CompoundTag serialize(){
        return CompoundTagBuilder.create()
                .withCompound("outputName", SerializeUtils.STRING.serialize(outputName))
                .withCompound("outputPortName", SerializeUtils.STRING.serialize(outputPortName))
                .withCompound("inputName", SerializeUtils.STRING.serialize(inputName))
                .withCompound("inputPortName", SerializeUtils.STRING.serialize(inputPortName))
                .build();
    }

    public static ConnectionNbt deserialize(CompoundTag tag){
        return new ConnectionNbt(
                SerializeUtils.STRING.deserialize(tag.getCompound("outputName")),
                SerializeUtils.STRING.deserialize(tag.getCompound("outputPortName")),
                SerializeUtils.STRING.deserialize(tag.getCompound("inputName")),
                SerializeUtils.STRING.deserialize(tag.getCompound("inputPortName"))
        );
    }

}
