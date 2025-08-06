package com.verr1.controlcraft.foundation.cimulink.core.components.vectors;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.Combinational;
import com.verr1.controlcraft.foundation.cimulink.core.registry.CimulinkFactory;
import com.verr1.controlcraft.foundation.cimulink.core.registry.Factory;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import org.joml.Quaterniond;
import org.joml.Quaterniondc;

import java.util.List;

import static com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils.nonNan;

public class Slerp extends Combinational {

    public Slerp() {
        super(
                List.of("qx", "qy", "qz", "qw", "tx", "ty", "tz", "tw", "ratio"),
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
        double qx = inputs.get(0);
        double qy = inputs.get(1);
        double qz = inputs.get(2);
        double qw = inputs.get(3);
        double tx = inputs.get(4);
        double ty = inputs.get(5);
        double tz = inputs.get(6);
        double tw = inputs.get(7);
        double ratio = inputs.get(8);

        Quaterniondc lerp = new Quaterniond(qx, qy, qz, qw)
                .slerp(new Quaterniond(tx, ty, tz, tw), ratio)
                .normalize();

        return List.of(
                nonNan(lerp.x()),
                nonNan(lerp.y()),
                nonNan(lerp.z()),
                nonNan(lerp.w())
        );
    }



    @Override
    public Factory<? extends NamedComponent> factory() {
        return CimulinkFactory.Q_SLERP;
    }

}
