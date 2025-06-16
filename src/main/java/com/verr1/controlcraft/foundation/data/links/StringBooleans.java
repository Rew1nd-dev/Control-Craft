package com.verr1.controlcraft.foundation.data.links;

import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.nbt.CompoundTag;

import java.util.List;

public record StringBooleans(List<StringBoolean> statuses) {
    public static final StringBooleans EMPTY = new StringBooleans(List.of());

    public static final SerializeUtils.Serializer<List<StringBoolean>> SER =
            SerializeUtils.ofList(SerializeUtils.of(
                    StringBoolean::serialize,
                    StringBoolean::deserialize
            ));

    public CompoundTag serialize(){
        return SER.serialize(statuses);
    }

    public static StringBooleans deserialize(CompoundTag tag){
        return new StringBooleans(SER.deserialize(tag));
    }


}
