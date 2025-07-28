package com.verr1.controlcraft.foundation.cimulink.core.components.sources;

import com.verr1.controlcraft.foundation.cimulink.core.components.general.Combinational;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;

import java.util.List;

public class MultiSource extends Combinational {


    public MultiSource(int n) {
        super(List.of(), ArrayUtils.createOutputNames(n));
    }

    public void setInput(int index, double val){
        ArrayUtils.AssertRange(index, m());
        updateOutput(index, val);
    }

    @Override
    protected List<Double> transform(List<Double> inputs) {
        return List.of();
    }
}
