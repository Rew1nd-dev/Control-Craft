package com.verr1.controlcraft.unstable.data;

import com.verr1.controlcraft.unstable.data.schematic.SchematicKey;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;

public class AIPersistentData {
    public static final Serializer<AIPersistentData> SER = SerializeUtils.of(
            AIPersistentData::serialize,
            AIPersistentData::deserialize
    );


    public final BlockPos center;
    public final SchematicKey key;

    public AIPersistentData(BlockPos center, SchematicKey key) {
        this.center = center;
        this.key = key;
    }

    public CompoundTag serialize(){
        return CompoundTagBuilder.create()
                .withCompound("pos", SerializeUtils.LONG.serialize(center.asLong()))
                .withCompound("key", key.serialize())
                .build();
    }

    public static AIPersistentData deserialize(CompoundTag tag){
        return new AIPersistentData(
                BlockPos.of(SerializeUtils.LONG.deserialize(tag.getCompound("pos"))),
                SchematicKey.deserialize(tag.getCompound("key"))
        );
    }

}
