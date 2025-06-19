package com.verr1.controlcraft.foundation.cimulink.core.components.general.da;

import com.verr1.controlcraft.foundation.cimulink.core.components.general.Combinational;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.utils.MathUtils;

import java.util.List;

public class AnalogDecoder8 extends Combinational {


    public AnalogDecoder8() {
        super(
                ArrayUtils.SINGLE_INPUT,
                ArrayUtils.flatten(
                        ArrayUtils.createOutputNames(8)
                )
        );
    }

    @Override
    protected List<Double> transform(List<Double> inputs) {
        double in = inputs.get(0);
        int floor = (int) MathUtils.clamp(in, 1e-4, 8 + 1e-4);
        List<Double> outs = ArrayUtils.ListOf(8, 0.0);

        boolean over = floor < 0 || floor >= 9;
        if(!over)outs.set(floor, 1.0);

        return outs;
    }
}
