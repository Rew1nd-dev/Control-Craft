package com.verr1.controlcraft.foundation.network.remote;

import com.verr1.controlcraft.utils.Serializer;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;

import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;

/*
*   T is task input
* */

public class RemotePort<T> {
    protected Class<T> type;

    protected BiConsumer<ServerPlayer, T> task;

    protected Function<T, CompoundTag> serializer;

    protected Function<CompoundTag, T> deserializer;

    protected RemotePort(Class<T> type, BiConsumer<ServerPlayer, T> task, Function<T, CompoundTag> serializer, Function<CompoundTag, T> deserializer) {
        this.type = type;
        this.task = task;
        this.serializer = serializer;
        this.deserializer = deserializer;
    }

    public static<T> RemotePort<T> of(Class<T> type, Consumer<T> task, Serializer<T> serializer) {
        return new RemotePort<>(
            type,
            ($, t) -> task.accept(t),
            serializer::serializeNullable,
            serializer::deserializeNullable
        );
    }

    public static<T> RemotePort<T> of(Class<T> type, BiConsumer<ServerPlayer, T> task, Serializer<T> serializer) {
        return new RemotePort<>(
            type,
            task,
            serializer::serializeNullable,
            serializer::deserializeNullable
        );
    }

    public CompoundTag serialize(Object input) {
        if (!type.isInstance(input)) {
            return new CompoundTag();
        }
        return serializer.apply(type.cast(input));
    }

    public void accept(ServerPlayer sender, Object object){
        if (!type.isInstance(object)) {
            return;
        }
        task.accept(sender, type.cast(object));
    }

    public T deserialize(CompoundTag tag) {
        return deserializer.apply(tag);
    }
}
