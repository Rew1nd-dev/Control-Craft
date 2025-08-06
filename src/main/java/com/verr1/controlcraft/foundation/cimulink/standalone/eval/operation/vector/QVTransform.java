package com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.vector;

import com.verr1.controlcraft.foundation.cimulink.core.components.vectors.LookAlong;
import com.verr1.controlcraft.foundation.cimulink.core.components.vectors.QTransform;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Evaluator;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Val;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.ValTransit;

import java.util.List;

public class QVTransform extends ValTransit {

    public QVTransform(
            Val qx, Val qy, Val qz, Val qw,
            Val vx, Val vy, Val vz,
            Evaluator evaluator
    ) {
        super(List.of(qx, qy, qz, qw, vx, vy, vz), new QTransform(), evaluator);
    }


    public Val x(){
        return outs().get(QTransform.x());
    }

    public Val y(){
        return outs().get(QTransform.y());
    }

    public Val z(){
        return outs().get(QTransform.z());
    }


}
