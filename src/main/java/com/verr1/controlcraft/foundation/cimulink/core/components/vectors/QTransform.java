package com.verr1.controlcraft.foundation.cimulink.core.components.vectors;

import com.verr1.controlcraft.foundation.cimulink.core.components.general.Combinational;

import java.util.List;

public class QTransform extends Combinational {

    public QTransform() {
        super(
                List.of("qx", "qy", "qz", "qw", "vx", "vy", "vz"),
                List.of("tx", "ty", "tz")
        );
    }

    @Override
    protected List<Double> transform(List<Double> inputs) {
        double qx = inputs.get(0);
        double qy = inputs.get(1);
        double qz = inputs.get(2);
        double qw = inputs.get(3);
        double vx = inputs.get(4);
        double vy = inputs.get(5);
        double vz = inputs.get(6);

        // Quaternion-vector multiplication
        double tx = qw * vx + qy * vz - qz * vy;
        double ty = qw * vy + qz * vx - qx * vz;
        double tz = qw * vz + qx * vy - qy * vx;

        return List.of(tx, ty, tz);
    }
}
