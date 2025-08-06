package com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.analog;

import com.verr1.controlcraft.foundation.cimulink.core.components.sources.DirectCurrent;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Evaluator;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.ValTransit;

import java.util.List;

public class Constant extends ValTransit {

    public Constant(double constant, Evaluator evaluator) {
        super(List.of(), new DirectCurrent(constant), evaluator);
    }
}
