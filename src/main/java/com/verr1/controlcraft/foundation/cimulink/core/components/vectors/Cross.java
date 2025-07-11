package com.verr1.controlcraft.foundation.cimulink.core.components.vectors;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.Combinational;
import com.verr1.controlcraft.foundation.cimulink.core.registry.CimulinkFactory;
import com.verr1.controlcraft.foundation.cimulink.core.registry.Factory;

import java.util.List;

public class Cross extends Combinational {

    public Cross() {
        super(
                List.of("x0", "y0", "z0", "x1", "y1", "z1"),
                List.of("cx", "cy", "cz")
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

        double cx = y0 * z1 - z0 * y1;
        double cy = z0 * x1 - x0 * z1;
        double cz = x0 * y1 - y0 * x1;

        return List.of(cx, cy, cz);
    }

    @Override
    public Factory<? extends NamedComponent> factory() {
        return CimulinkFactory.V_CROSS;
    }
}
