package com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation;

import com.verr1.controlcraft.foundation.cimulink.core.components.digital.gates.Gates;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.ValTransit;

import java.util.List;
import java.util.function.Function;

public class LogicOperations {

    public static final MultiValFactory AND = wrap(Gates.AND);
    public static final MultiValFactory OR = wrap(Gates.OR);
    public static final MultiValFactory XOR = wrap(Gates.XOR);
    public static final MultiValFactory NAND = wrap(Gates.NAND);
    public static final MultiValFactory NOT = wrap(Gates.NOT);



    private static MultiValFactory wrap(Function<Integer, Gates.Gate> gateFunc){
        return (evaluator, vals) -> new ValTransit(
                List.of(vals),
                gateFunc.apply(vals.length),
                evaluator
        );
    }

}
