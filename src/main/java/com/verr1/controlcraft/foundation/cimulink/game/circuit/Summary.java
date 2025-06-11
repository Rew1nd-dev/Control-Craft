package com.verr1.controlcraft.foundation.cimulink.game.circuit;

import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.nbt.CompoundTag;

public record Summary(
        String registerName,
        CompoundTag componentTag
) {

    public CompoundTag serialize(){
        return CompoundTagBuilder.create()
                .withCompound("registeredName", SerializeUtils.STRING.serialize(registerName))
                .withCompound("componentTag", componentTag)
                .build();
    }

    public static Summary deserialize(CompoundTag tag){
        return new Summary(
                SerializeUtils.STRING.deserialize(tag.getCompound("registeredName")),
                tag.getCompound("componentTag")
        );
    }


}
