package com.verr1.controlcraft.utils;

import net.minecraft.nbt.CompoundTag;
import org.jetbrains.annotations.NotNull;

import javax.annotation.Nullable;

public interface Serializer<T> {


    default CompoundTag serializeNullable(@Nullable T obj) {
        return obj == null ? new CompoundTag() : serialize(obj);
    }

    default @Nullable T deserializeNullable(CompoundTag tag) {
        return tag.isEmpty() ? null : deserialize(tag);
    }

    CompoundTag serialize(@NotNull T obj);

    @NotNull
    T deserialize(CompoundTag tag);

    default T deserializeOrElse(CompoundTag tag, T orElse) {
        if (tag.isEmpty()) return orElse;
        return deserialize(tag);
    }

}
