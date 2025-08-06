package com.verr1.controlcraft.foundation.cimulink.standalone.eval;

import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.Circuit;

public class Submodule {

    public final Evaluator evaluator;

    public Submodule(Evaluator evaluator) {
        this.evaluator = evaluator;
    }

    public Circuit build(){
        return evaluator.evaluate().buildContext().buildCircuit();
    }

}
