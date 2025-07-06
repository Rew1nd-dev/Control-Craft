package com.verr1.controlcraft.foundation.cimulink.core.components.digital.gates;

import com.verr1.controlcraft.foundation.cimulink.core.components.digital.BooleanCombinational;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import net.minecraft.nbt.CompoundTag;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.IntStream;

public class FlexibleGate extends BooleanCombinational {

    public static final Serializer<List<Boolean>> SER = SerializeUtils.ofList(SerializeUtils.BOOLEAN);

    public final List<Boolean> mask;
    public final List<Boolean> maskView;
    public boolean outputMask = true;
    public boolean isAndGate = true;

    public FlexibleGate(int n) {
        super(
                ArrayUtils.createInputNames(n),
                ArrayUtils.SINGLE_OUTPUT
        );
        this.mask = new ArrayList<>(ArrayUtils.ListOf(n, false));
        maskView = Collections.unmodifiableList(mask);
    }


    public FlexibleGate convertTo(int n){
        FlexibleGate newGate = new FlexibleGate(n);
        newGate.setMask(mask.subList(0, Math.min(mask.size(), n)));
        newGate.setOutputMask(outputMask);
        newGate.setAndGate(isAndGate);
        return newGate;
    }

    public List<Boolean> viewMask(){
        return maskView;
    }

    public void setAndGate(boolean isAndGate) {
        this.isAndGate = isAndGate;
    }

    public boolean isAndGate() {
        return isAndGate;
    }

    public void setMask(List<Boolean> newMask){
        for (int i = 0; i < Math.min(mask.size(), newMask.size()); i++) {
            mask.set(i, newMask.get(i));
        }
    }

    public void setMask(int index, boolean value) {
        if (index >= 0 && index < mask.size()) {
            mask.set(index, value);
        }
    }

    public boolean outputMask(){
        return outputMask;
    }

    public void setOutputMask(boolean outputMask) {
        this.outputMask = outputMask;
    }

    @Override
    protected List<Boolean> transformBoolean(List<Boolean> inputs) {
        ArrayUtils.AssertSameSize(inputs, mask);
        return isAndGate
                ? transformAnd(inputs)
                : transformOr(inputs);
    }

    protected List<Boolean> maskIn(List<Boolean> inputs){
        return IntStream.range(0, n()).mapToObj(i -> inputs.get(i) ^ mask.get(i))
                .toList();
    }

    protected List<Boolean> transformAnd(List<Boolean> inputs) {
        boolean out = outputMask ^ maskIn(inputs).stream().reduce(true, (a, b) -> a && b);
        return List.of(out);
    }

    protected List<Boolean> transformOr(List<Boolean> inputs) {
        boolean out = outputMask ^ maskIn(inputs).stream().reduce(false, (a, b) -> a || b);
        return List.of(out);
    }

    public CompoundTag serialize(){
        return CompoundTagBuilder.create()
                .withCompound("mask", SER.serialize(mask))
                .withCompound("outputMask", SerializeUtils.BOOLEAN.serialize(outputMask))
                .withCompound("isAndGate", SerializeUtils.BOOLEAN.serialize(isAndGate))
                .build();
    }

    public static FlexibleGate deserialize(CompoundTag tag) {
        List<Boolean> mask = SER.deserialize(tag.getCompound("mask"));
        boolean outputMask = SerializeUtils.BOOLEAN.deserialize(tag.getCompound("outputMask"));
        boolean isAndGate = SerializeUtils.BOOLEAN.deserialize(tag.getCompound("isAndGate"));

        FlexibleGate gate = new FlexibleGate(mask.size());
        gate.setMask(mask);
        gate.setOutputMask(outputMask);
        gate.setAndGate(isAndGate);

        return gate;
    }

}
