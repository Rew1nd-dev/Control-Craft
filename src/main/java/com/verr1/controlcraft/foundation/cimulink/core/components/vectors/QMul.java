package com.verr1.controlcraft.foundation.cimulink.core.components.vectors;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.Combinational;
import com.verr1.controlcraft.foundation.cimulink.core.registry.CimulinkFactory;
import com.verr1.controlcraft.foundation.cimulink.core.registry.Factory;
import org.joml.Quaterniond;

import java.util.List;

public class QMul extends Combinational {

    public QMul() {
        super(
                List.of("qx0", "qy0", "qz0", "qw0", "qx1", "qy1", "qz1", "qw1"),
                List.of("x", "y", "z", "w")
        );
    }

    @Override
    protected List<Double> transform(List<Double> inputs) {
        double qx0 = inputs.get(0);
        double qy0 = inputs.get(1);
        double qz0 = inputs.get(2);
        double qw0 = inputs.get(3);
        double qx1 = inputs.get(4);
        double qy1 = inputs.get(5);
        double qz1 = inputs.get(6);
        double qw1 = inputs.get(7);

        // Quaternion multiplication
        Quaterniond res =
                        new Quaterniond(qx0, qy0, qz0, qw0)
                .mul(
                        new Quaterniond(qx1, qy1, qz1, qw1)
                );

        return List.of(res.x(), res.y(), res.z(), res.w());
    }

    @Override
    public Factory<? extends NamedComponent> factory() {
        return CimulinkFactory.Q_MUL;
    }

}
