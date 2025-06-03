package com.verr1.controlcraft.foundation.data.links;

import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.nbt.CompoundTag;

public record BlockPort(WorldBlockPos pos, String portName) {

    public CompoundTag serialize(){
        return CompoundTagBuilder.create()
                .withCompound("pos", pos.serialize())
                .withCompound("port_name", SerializeUtils.STRING.serialize(portName))
                .build();
    }

    public static BlockPort deserialize(CompoundTag tag){
        return new BlockPort(
                WorldBlockPos.deserialize(tag.getCompound("pos")),
                SerializeUtils.STRING.deserialize(tag.getCompound("port_name"))
        );
    }

}
