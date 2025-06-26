package com.verr1.controlcraft.foundation.cimulink.core.components.vectors;

import com.verr1.controlcraft.foundation.cimulink.core.components.general.Combinational;

import java.util.List;

public class Dot extends Combinational {


    public Dot() {
        super(
                List.of("x0", "y0", "z0", "x1", "y1", "z1"),
                List.of("dot")
        );
    }

    @Override
    protected List<Double> transform(List<Double> inputs) {
        double x0 = inputs.get(0);
        double y0 = inputs.get(1);
        double z0 = inputs.get(2);
        double x1 = inputs.get(3);
        double y1 = inputs.get(4);
        double z1 = inputs.get(5);

        double dotProduct = x0 * x1 + y0 * y1 + z0 * z1;
        return List.of(dotProduct);
    }
}
