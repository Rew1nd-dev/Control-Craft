package com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.vector;

import com.verr1.controlcraft.foundation.cimulink.core.components.vectors.Dot;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Evaluator;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Val;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.ValTransit;

import java.util.List;

public class VDot extends ValTransit {


    public VDot(
            Val x0, Val y0, Val z0,
            Val x1, Val y1, Val z1,
            Evaluator evaluator
    ) {
        super(List.of(x0, y0, z0, x1, y1, z1), new Dot(), evaluator);
    }



}
