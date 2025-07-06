package com.verr1.controlcraft.foundation.data.links;

import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import net.minecraft.nbt.CompoundTag;

import java.util.List;

public record StringBooleans(List<StringBoolean> statuses) {
    public static final StringBooleans EMPTY = new StringBooleans(List.of());

    public static final Serializer<List<StringBoolean>> SER =
            SerializeUtils.ofList(SerializeUtils.of(
                    StringBoolean::serialize,
                    StringBoolean::deserialize
            ));

    public static final Serializer<StringBooleans> SERIALIZER =
            SerializeUtils.of(
                    StringBooleans::serialize,
                    StringBooleans::deserialize
            );

    public CompoundTag serialize(){
        return SER.serialize(statuses);
    }

    public static StringBooleans deserialize(CompoundTag tag){
        return new StringBooleans(SER.deserialize(tag));
    }

    public List<String> strings() {
        return statuses.stream()
                .map(StringBoolean::name)
                .toList();
    }

    public List<Boolean> booleans() {
        return statuses.stream()
                .map(StringBoolean::enabled)
                .toList();
    }

}
