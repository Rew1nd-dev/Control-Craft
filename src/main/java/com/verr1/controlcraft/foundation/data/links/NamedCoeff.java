package com.verr1.controlcraft.foundation.data.links;

import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import kotlin.Pair;
import net.minecraft.nbt.CompoundTag;

public record NamedCoeff(String name, Double coeff) {
    public CompoundTag serialize() {
        return CompoundTagBuilder.create()
                .withCompound("name", SerializeUtils.STRING.serialize(name))
                .withCompound("coeff", SerializeUtils.DOUBLE.serialize(coeff))
                .build();
    }

    public static NamedCoeff deserialize(CompoundTag tag) {
        String name = SerializeUtils.STRING.deserialize(tag.getCompound("name"));
        Double coeff = SerializeUtils.DOUBLE.deserialize(tag.getCompound("coeff"));
        return new NamedCoeff(name, coeff);
    }

    public Pair<String, Double> mapToPair() {
        return new Pair<>(name, coeff);
    }

    public static NamedCoeff fromPair(Pair<String, Double> pair) {
        return new NamedCoeff(pair.getFirst(), pair.getSecond());
    }
}
