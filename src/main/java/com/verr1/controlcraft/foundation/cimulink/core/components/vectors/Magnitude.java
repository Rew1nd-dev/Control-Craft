package com.verr1.controlcraft.foundation.cimulink.core.components.vectors;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.Combinational;
import com.verr1.controlcraft.foundation.cimulink.core.registry.CimulinkFactory;
import com.verr1.controlcraft.foundation.cimulink.core.registry.Factory;

import java.util.List;

public class Magnitude extends Combinational {

    public Magnitude() {
        super(
                List.of("x", "y", "z"),
                List.of("mag")
        );
    }

    @Override
    protected List<Double> transform(List<Double> inputs) {
        double x0 = inputs.get(0);
        double y0 = inputs.get(1);
        double z0 = inputs.get(2);

        return List.of(Math.sqrt(x0 * x0 + y0 * y0 + z0 * z0));
    }

    @Override
    public Factory<? extends NamedComponent> factory() {
        return CimulinkFactory.V_MAG;
    }
}
