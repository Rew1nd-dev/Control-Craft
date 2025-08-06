package com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.analog;

import com.verr1.controlcraft.foundation.cimulink.core.components.general.ad.Comparator;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Evaluator;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Val;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.ValTransit;

import java.util.List;

public class Compare extends ValTransit {

    public Compare(Val a, Val b, Evaluator evaluator) {
        super(List.of(a, b), new Comparator(), evaluator);
    }

    Comparator cast(){
        return (Comparator) component();
    }

    public Val ge(){
        return outs().get(cast().ge());
    }

    public Val le(){
        return outs().get(cast().le());
    }

    public Val eq(){
        return outs().get(cast().eq());
    }

}
