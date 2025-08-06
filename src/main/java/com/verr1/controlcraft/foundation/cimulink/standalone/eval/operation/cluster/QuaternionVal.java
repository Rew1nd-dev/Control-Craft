package com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.cluster;

import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Evaluator;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Val;

public record QuaternionVal(Val x, Val y, Val z, Val w, Evaluator eval) {

    public QuaternionVal conj(){
        return eval.conjugate(this);
    }

    public Vector3Val transform(Vector3Val v){
        return eval.transform(this, v);
    }

    public QuaternionVal ifZeroThenUnit(Val zero, Val one){
        Val cond = eval.mag(x, y, z, w).lessThan(1e-5);
        return new QuaternionVal(
                eval.orElse(cond, zero, x),
                eval.orElse(cond, zero, y),
                eval.orElse(cond, zero, z),
                eval.orElse(cond, one, w),
                eval
        );
    }

}
