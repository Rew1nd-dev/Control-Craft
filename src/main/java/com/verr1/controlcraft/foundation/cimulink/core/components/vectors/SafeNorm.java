package com.verr1.controlcraft.foundation.cimulink.core.components.vectors;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.Combinational;
import com.verr1.controlcraft.foundation.cimulink.core.registry.CimulinkFactory;
import com.verr1.controlcraft.foundation.cimulink.core.registry.Factory;

import java.util.List;

public class SafeNorm extends Combinational {

    public SafeNorm() {
        super(
                List.of("x", "y", "z"),
                List.of("nx", "ny", "nz")
        );
    }

    @Override
    protected List<Double> transform(List<Double> inputs) {
        double x0 = inputs.get(0);
        double y0 = inputs.get(1);
        double z0 = inputs.get(2);

        double magnitude = Math.sqrt(x0 * x0 + y0 * y0 + z0 * z0);
        if (magnitude < 1e-6) {
            // Avoid division by zero, return zero vector
            return List.of(0.0, 0.0, 0.0);
        }

        return List.of(x0 / magnitude, y0 / magnitude, z0 / magnitude);
    }

    @Override
    public Factory<? extends NamedComponent> factory() {
        return CimulinkFactory.V_NORM;
    }

}
