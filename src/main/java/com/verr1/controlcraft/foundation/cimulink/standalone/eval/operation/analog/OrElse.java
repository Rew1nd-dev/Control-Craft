package com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.analog;

import com.verr1.controlcraft.foundation.cimulink.core.components.general.da.Multiplexer;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Evaluator;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Val;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.ValTransit;

import java.util.Map;

public class OrElse extends ValTransit {
    private static final Multiplexer MUX = new Multiplexer(1);

    public OrElse(Val condition, Val trueVal, Val falseVal, Evaluator evaluator) {
        super(
                Map.of(
                        MUX.sel(0), condition,
                        MUX.dat(0), falseVal,
                        MUX.dat(1), trueVal
                ),
                new Multiplexer(1),
                evaluator
        );
    }

}
