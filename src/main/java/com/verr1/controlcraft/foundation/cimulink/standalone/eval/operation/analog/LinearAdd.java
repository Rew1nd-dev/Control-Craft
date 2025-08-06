package com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.analog;

import com.verr1.controlcraft.foundation.cimulink.core.components.analog.LinearAdder;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Evaluator;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Val;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.ValTransit;

import java.util.List;

public class LinearAdd extends ValTransit {

    public LinearAdd(
            double coeffA, Val a,
            double coeffB, Val b,
            Evaluator evaluator
    ) {
        super(List.of(a, b), new LinearAdder(List.of(coeffA, coeffB)), evaluator);
    }

    public LinearAdd(
            double[] coeffs,
            Val[] vals,
            Evaluator evaluator
    ) {
        super(List.of(vals), new LinearAdder(coeffs), evaluator);
        if (coeffs.length != vals.length) {
            throw new IllegalArgumentException("Coefficient and value arrays must have the same length.");
        }
    }

}
