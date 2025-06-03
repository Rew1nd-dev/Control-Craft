package com.verr1.controlcraft.foundation.data.links;

import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.nbt.CompoundTag;

import java.util.ArrayList;
import java.util.List;

public class ValueStatus {
    public static final SerializeUtils.Serializer<List<Double>> VALUES =
            SerializeUtils.ofList(SerializeUtils.DOUBLE);

    public final List<Double> inputValues = new ArrayList<>();
    public final List<Double> outputValues = new ArrayList<>();

    public ValueStatus() {
    }

    public ValueStatus(
            List<Double> inputValues,
            List<Double> outputValues
    ) {
        this.inputValues.addAll(inputValues);
        this.outputValues.addAll(outputValues);
    }

    public static CompoundTag summarize(BlockLinkPort blp){
        return CompoundTagBuilder.create()
                .withCompound("inputValues", VALUES.serialize(blp.inputs()))
                .withCompound("outputValues", VALUES.serialize(blp.outputs()))
                .build();
    }

    public static ValueStatus deserialize(CompoundTag tag){
        return new ValueStatus(
                VALUES.deserialize(tag.getCompound("inputValues")),
                VALUES.deserialize(tag.getCompound("outputValues"))
        );
    }
}
