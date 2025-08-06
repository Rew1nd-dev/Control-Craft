package com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.vector;

import com.verr1.controlcraft.foundation.cimulink.core.components.vectors.Cross;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Evaluator;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Val;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.ValTransit;

import java.util.List;

public class VCross extends ValTransit {

    public VCross(Val x0, Val y0, Val z0,
                  Val x1, Val y1, Val z1,
                  Evaluator evaluator
    ) {
        super(List.of(x0, y0, z0, x1, y1, z1), new Cross(), evaluator);
    }


    public Val x(){
        return outs().get(Cross.x());
    }

    public Val y(){
        return outs().get(Cross.y());
    }

    public Val z(){
        return outs().get(Cross.z());
    }

}
