package com.verr1.controlcraft.unstable.data.v1;

import com.verr1.controlcraft.unstable.data.schematic.SchematicKey;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;

public class AIPersistentDataV1 {
    public static final Serializer<AIPersistentDataV1> SER = SerializeUtils.of(
            AIPersistentDataV1::serialize,
            AIPersistentDataV1::deserialize
    );


    public final BlockPos center;
    public final SchematicKey key;

    public AIPersistentDataV1(BlockPos center, SchematicKey key) {
        this.center = center;
        this.key = key;
    }

    public CompoundTag serialize(){
        return CompoundTagBuilder.create()
                .withCompound("pos", SerializeUtils.LONG.serialize(center.asLong()))
                .withCompound("key", key.serialize())
                .build();
    }

    public static AIPersistentDataV1 deserialize(CompoundTag tag){
        return new AIPersistentDataV1(
                BlockPos.of(SerializeUtils.LONG.deserialize(tag.getCompound("pos"))),
                SchematicKey.deserialize(tag.getCompound("key"))
        );
    }

}
