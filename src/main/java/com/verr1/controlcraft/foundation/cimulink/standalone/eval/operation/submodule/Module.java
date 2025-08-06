package com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.submodule;

import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.Circuit;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Evaluator;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Val;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.ValTransit;

import java.util.Map;

public class Module extends ValTransit {
    public Module(Map<String, Val> inVals, Circuit module, Evaluator evaluator) {
        super(inVals, module, evaluator);
    }
}
