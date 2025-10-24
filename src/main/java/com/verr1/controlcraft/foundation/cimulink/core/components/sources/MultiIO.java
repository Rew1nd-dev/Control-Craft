package com.verr1.controlcraft.foundation.cimulink.core.components.sources;

import com.verr1.controlcraft.foundation.cimulink.core.components.general.Combinational;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;

import java.util.List;

public class MultiIO extends Combinational {


    public MultiIO(int n) {
        super(ArrayUtils.createInputNames(n), ArrayUtils.createOutputNames(n));
    }

    public void setToCircuit(int index, double val){
        ArrayUtils.AssertRange(index, m());
        updateOutput(index, val);
    }

    public double getFromCircuit(int index){
        ArrayUtils.AssertRange(index, n());
        return peekInput(index);
    }

    @Override
    protected void updateOutput(List<Double> outputValues) {

    }

    @Override
    protected List<Double> transform(List<Double> inputs) {
        // nothing, cc act as transform()
        return ArrayUtils.ListOf(n(), 0.0); // return zeroes and avoid updateOutput throw an IllegalArgumentException
    }
}
