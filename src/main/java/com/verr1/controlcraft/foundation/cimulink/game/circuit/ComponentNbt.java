package com.verr1.controlcraft.foundation.cimulink.game.circuit;

import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.nbt.CompoundTag;

public record ComponentNbt(String componentName, Summary componentTag) {

    public CompoundTag serialize(){
        return CompoundTagBuilder.create()
                .withCompound("name", SerializeUtils.STRING.serialize(componentName))
                .withCompound("summary", componentTag.serialize())
                .build();
    }

    public static ComponentNbt deserialize(CompoundTag tag){
        return new ComponentNbt(
                SerializeUtils.STRING.deserialize(tag.getCompound("name")),
                Summary.deserialize(tag.getCompound("summary"))
        );
    }

}
