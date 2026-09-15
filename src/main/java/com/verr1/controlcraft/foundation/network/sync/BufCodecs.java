package com.verr1.controlcraft.foundation.network.sync;

import net.minecraft.network.FriendlyByteBuf;

import java.util.HashMap;
import java.util.Map;

public class BufCodecs {
    private static final Map<Class<?>, BufCodec<?>> ENUM_CODECS = new HashMap<>();

    public static final BufCodec<Double> DOUBLE = BufCodec.of(FriendlyByteBuf::writeDouble, FriendlyByteBuf::readDouble);
    public static final BufCodec<Float> FLOAT = BufCodec.of(FriendlyByteBuf::writeFloat, FriendlyByteBuf::readFloat);
    public static final BufCodec<Integer> INT = BufCodec.of(FriendlyByteBuf::writeVarInt, FriendlyByteBuf::readVarInt);
    public static final BufCodec<Long> LONG = BufCodec.of(FriendlyByteBuf::writeVarLong, FriendlyByteBuf::readVarLong);
    public static final BufCodec<Boolean> BOOLEAN = BufCodec.of(FriendlyByteBuf::writeBoolean, FriendlyByteBuf::readBoolean);
    public static final BufCodec<String> STRING = BufCodec.of(FriendlyByteBuf::writeUtf, FriendlyByteBuf::readUtf);

    private BufCodecs() {
    }

    @SuppressWarnings("unchecked")
    public static <T extends Enum<T>> BufCodec<T> ofEnum(Class<T> enumClass) {
        return (BufCodec<T>) ENUM_CODECS.computeIfAbsent(
                enumClass,
                ignored -> BufCodec.of(
                        FriendlyByteBuf::writeEnum,
                        buffer -> buffer.readEnum(enumClass)
                )
        );
    }
}
