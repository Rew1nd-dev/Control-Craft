package com.verr1.controlcraft.foundation.cimulink.core.components.vectors;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.Combinational;
import com.verr1.controlcraft.foundation.cimulink.core.registry.CimulinkFactory;
import com.verr1.controlcraft.foundation.cimulink.core.registry.Factory;
import org.joml.Quaterniond;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.List;

public class QTransform extends Combinational {

    public QTransform() {
        super(
                List.of("qx", "qy", "qz", "qw", "vx", "vy", "vz"),
                List.of(x(), y(), z())
        );
    }

    public static String x(){
        return "tx";
    }

    public static String y(){
        return "ty";
    }

    public static String z(){
        return "tz";
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
        Vector3dc vt = new Quaterniond(qx, qy, qz, qw).transform(vx, vy, vz, new Vector3d());

        return List.of(vt.x(), vt.y(), vt.z());
    }

    @Override
    public Factory<? extends NamedComponent> factory() {
        return CimulinkFactory.V_TRANSFORM;
    }
}
