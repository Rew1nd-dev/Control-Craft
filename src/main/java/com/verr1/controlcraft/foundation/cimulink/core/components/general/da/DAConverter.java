package com.verr1.controlcraft.foundation.cimulink.core.components.general.da;



import com.verr1.controlcraft.foundation.cimulink.core.components.general.Combinational;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;

import java.util.List;

public class DAConverter extends Combinational {

    private double min;
    private double max;

    public DAConverter(int n, double min, double max) {
        super(
                ArrayUtils.createInputNames(n),
                ArrayUtils.createOutputNames(n)
        );
        this.min = min;
        this.max = max;
    }

    @Override
    protected List<Double> transform(List<Double> inputs) {
        return ArrayUtils.mapToList(
                inputs,
                d -> min + (max - min) * d
        );
    }
}
