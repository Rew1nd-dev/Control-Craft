package com.verr1.controlcraft.foundation.network.sync;

import net.minecraft.network.FriendlyByteBuf;

@FunctionalInterface
public interface BufCodec<T> {

    void encode(FriendlyByteBuf buffer, T value);

    default T decode(FriendlyByteBuf buffer) {
        throw new UnsupportedOperationException("Decode is not implemented for this codec.");
    }

    static <T> BufCodec<T> of(Encoder<T> encoder, Decoder<T> decoder) {
        return new BufCodec<>() {
            @Override
            public void encode(FriendlyByteBuf buffer, T value) {
                encoder.encode(buffer, value);
            }

            @Override
            public T decode(FriendlyByteBuf buffer) {
                return decoder.decode(buffer);
            }
        };
    }

    @FunctionalInterface
    interface Encoder<T> {
        void encode(FriendlyByteBuf buffer, T value);
    }

    @FunctionalInterface
    interface Decoder<T> {
        T decode(FriendlyByteBuf buffer);
    }
}
