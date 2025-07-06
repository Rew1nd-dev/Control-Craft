package com.verr1.controlcraft.foundation.data.links;

import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import net.minecraft.nbt.CompoundTag;

import java.util.List;

public record StringBooleanDoubles (List<StringBooleanDouble> contents){

    public static final Serializer<List<StringBooleanDouble>> SER =
            SerializeUtils.ofList(SerializeUtils.of(
                    StringBooleanDouble::serialize,
                    StringBooleanDouble::deserialize
            ));

    public static final StringBooleanDoubles EMPTY = new StringBooleanDoubles(List.of());

    public CompoundTag serialize() {
        return SER.serialize(contents);
    }

    public static StringBooleanDoubles deserialize(CompoundTag tag) {
        return new StringBooleanDoubles(SER.deserialize(tag));
    }

}
