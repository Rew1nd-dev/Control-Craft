package com.verr1.controlcraft.foundation.cimulink.core.components.general;



import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;

import java.util.List;
import java.util.stream.IntStream;

public abstract class Combinational extends NamedComponent {
    public final List<Integer> allOutputs;

    public Combinational(List<String> inputs, List<String> outputs) {
        super(inputs, outputs);
        allOutputs = IntStream.range(0, m()).boxed().toList();
    }

    @Override
    public final void onInputChange(Integer... indexes) {
        if(indexes.length == 0)return;
        updateOutput(transform(retrieveInput()));
    }

    @Override
    public List<Integer> propagateTo(int $) {
        return allOutputs;
    }

    @Override
    public final void onPositiveEdge() {}



    protected abstract List<Double> transform(List<Double> inputs);

}
