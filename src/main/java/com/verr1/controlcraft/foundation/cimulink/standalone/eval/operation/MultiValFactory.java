package com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation;

import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Evaluator;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Val;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.ValTransit;

@FunctionalInterface
public interface MultiValFactory {

    ValTransit create(Evaluator evaluator, Val... vals);

}
