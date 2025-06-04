package com.verr1.controlcraft.foundation.cimulink.core.components.sources;

import com.verr1.controlcraft.foundation.cimulink.core.components.general.Combinational;

import java.util.List;

public class Sink extends Combinational {


    public Sink() {
        super(List.of("in"), List.of());
    }

    @Override
    protected List<Double> transform(List<Double> inputs) {
        return List.of();
    }
}
