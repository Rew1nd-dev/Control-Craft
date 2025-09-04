package com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.cluster;

import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Evaluator;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Val;

import java.util.List;

public record Vector3Val(Val x, Val y, Val z, Evaluator eval) {

    public Vector3Val normalize(){
        List<Val> n = eval.norm(x, y, z);
        return new Vector3Val(n.get(0), n.get(1), n.get(2), eval);
    }

    public Val lengthSquare(){
        return eval.add(x.mul(x), y.mul(y), z.mul(z));
    }

    public Vector3Val sub(Vector3Val other){
        return new Vector3Val(
                x.sub(other.x),
                y.sub(other.y),
                z.sub(other.z),
                eval
        );
    }

    public Val length(){
        return eval.sqrt(lengthSquare());
    }

    public Val dot(Vector3Val other){
        return eval.dot(this, other);
    }

    public Vector3Val scale(Val s){
        return new Vector3Val(x.mul(s), y.mul(s), z.mul(s), eval);
    }

    public Vector3Val add(Vector3Val other) {
        return new Vector3Val(
                x.add(other.x),
                y.add(other.y),
                z.add(other.z),
                eval
        );
    }
}
