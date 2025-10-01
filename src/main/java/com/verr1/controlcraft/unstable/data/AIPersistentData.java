package com.verr1.controlcraft.unstable.data;

import com.verr1.controlcraft.unstable.data.schematic.SchematicKey;
import com.verr1.controlcraft.unstable.data.v1.AIPersistentDataV1;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import org.jetbrains.annotations.NotNull;

import java.util.Objects;

public class AIPersistentData {

    public static final Serializer<AIPersistentData> SER = SerializeUtils.of(
            AIPersistentData::serialize,
            AIPersistentData::deserialize
    );


    public final @NotNull BlockPos center;
    public final @NotNull SchematicKey storageSchematic;

    public AIPersistentData(@NotNull BlockPos center, @NotNull SchematicKey key) {
        this.center = Objects.requireNonNull(center);
        this.storageSchematic = Objects.requireNonNull(key);
    }

    public CompoundTag serialize(){
        return CompoundTagBuilder.create()
                .withCompound("pos", SerializeUtils.LONG.serialize(center.asLong()))
                .withCompound("key", storageSchematic.serialize())
                .build();
    }

    public static AIPersistentData deserialize(CompoundTag tag){
        return new AIPersistentData(
                BlockPos.of(SerializeUtils.LONG.deserialize(tag.getCompound("pos"))),
                SchematicKey.deserialize(tag.getCompound("key"))
        );
    }

}
