package com.verr1.controlcraft.foundation.data.links;

import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.nbt.CompoundTag;

public record PortStatus(String name, boolean enabled) {

    public CompoundTag serialize(){
        return CompoundTagBuilder.create()
                .withCompound("name", SerializeUtils.STRING.serialize(name))
                .withCompound("enabled", SerializeUtils.BOOLEAN.serialize(enabled))
                .build();
    }

    public static PortStatus deserialize(CompoundTag tag){
        return new PortStatus(
                SerializeUtils.STRING.deserialize(tag.getCompound("name")),
                SerializeUtils.BOOLEAN.deserialize(tag.getCompound("enabled"))
        );
    }



}
