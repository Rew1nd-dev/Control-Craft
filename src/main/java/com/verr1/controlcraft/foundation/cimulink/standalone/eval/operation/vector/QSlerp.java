package com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.vector;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.vectors.Cross;
import com.verr1.controlcraft.foundation.cimulink.core.components.vectors.Slerp;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Evaluator;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Val;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.ValTransit;

import java.util.List;

public class QSlerp extends ValTransit {

    public QSlerp(Val x0, Val y0, Val z0, Val w0,
                  Val x1, Val y1, Val z1, Val w1,
                  Val rate,
                  Evaluator evaluator
    ) {
        super(List.of(x0, y0, z0, w0, x1, y1, z1, w1, rate), new Slerp(), evaluator);
    }


    public Val x(){
        return outs().get(Slerp.x());
    }

    public Val y(){
        return outs().get(Slerp.y());
    }

    public Val z(){
        return outs().get(Slerp.z());
    }

    public Val w(){
        return outs().get(Slerp.w());
    }

}
