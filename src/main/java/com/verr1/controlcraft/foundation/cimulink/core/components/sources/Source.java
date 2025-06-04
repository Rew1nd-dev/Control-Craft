package com.verr1.controlcraft.foundation.cimulink.core.components.sources;

import com.verr1.controlcraft.foundation.cimulink.core.components.general.Combinational;

import java.util.List;

public class Source extends Combinational {

    public Source() {
        super(List.of(), List.of("out"));
    }

    public void setInput(double val){
        updateOutput(0, val);
    }

    @Override
    protected List<Double> transform(List<Double> inputs) {
        return List.of();
    }
}
