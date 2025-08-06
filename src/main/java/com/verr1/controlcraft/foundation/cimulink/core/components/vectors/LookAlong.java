package com.verr1.controlcraft.foundation.cimulink.core.components.vectors;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.Combinational;
import com.verr1.controlcraft.foundation.cimulink.core.registry.CimulinkFactory;
import com.verr1.controlcraft.foundation.cimulink.core.registry.Factory;
import org.joml.Quaterniond;
import org.joml.Vector3d;

import java.util.List;

public class LookAlong extends Combinational {


    public LookAlong() {
        super(
                List.of("dirX", "dirY", "dirZ", "upX", "upY", "upZ"),
                List.of(x(), y(), z(), w())
        );
    }

    public static String x(){
        return "x";
    }

    public static String y(){
        return "y";
    }

    public static String z(){
        return "z";
    }

    public static String w(){
        return "w";
    }

    @Override
    protected List<Double> transform(List<Double> inputs) {
        double x0 = inputs.get(0);
        double y0 = inputs.get(1);
        double z0 = inputs.get(2);
        double x1 = inputs.get(3);
        double y1 = inputs.get(4);
        double z1 = inputs.get(5);

        Quaterniond look = new Quaterniond().lookAlong(x0, y0, z0, x1, y1, z1);
        return List.of(look.x(), look.y(), look.z(), look.w());
    }

    @Override
    public Factory<? extends NamedComponent> factory() {
        return CimulinkFactory.Q_LOOK_ALONG;
    }
}
