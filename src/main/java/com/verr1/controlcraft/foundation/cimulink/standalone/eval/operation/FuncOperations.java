package com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation;

import com.verr1.controlcraft.foundation.cimulink.core.components.analog.Functions;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.ValTransit;

import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

public class FuncOperations {

    public static final MultiValFactory MAX = wrap(Functions.MAX);
    public static final MultiValFactory MIN = wrap(Functions.MIN);
    public static final MultiValFactory MUL = wrap(Functions.PRODUCT);
    public static final OneValFactory ANGLE_FIX = oneVal(Functions.ANGLE_FIX);
    public static final OneValFactory RAD = oneVal(Functions.RAD);
    public static final OneValFactory DEG = oneVal(Functions.DEG);
    public static final OneValFactory ABS = oneVal(Functions.ABS);
    public static final OneValFactory SIN = oneVal(Functions.SIN);
    public static final OneValFactory COS = oneVal(Functions.COS);
    public static final OneValFactory TAN = oneVal(Functions.TAN);
    public static final OneValFactory ASIN = oneVal(Functions.ASIN);
    public static final OneValFactory ACOS = oneVal(Functions.ACOS);
    public static final TwoValFactory ATAN = twoVal(Functions.ATAN);
    public static final TwoValFactory POWER = twoVal(Functions.POWER); // a**b
    public static final TwoValFactory DIV = twoVal(Functions.DIV);


    private static MultiValFactory wrap(Function<Integer, Functions.FunctionN> func) {
        return (evaluator, vals) -> new ValTransit(
                List.of(vals),
                func.apply(vals.length),
                evaluator
        );
    }

    private static TwoValFactory twoVal(Supplier<Functions.FunctionN> func) {
        return (evaluator, a, b) -> new ValTransit(
                List.of(a, b),
                func.get(),
                evaluator
        );
    }

    private static OneValFactory oneVal(Supplier<Functions.FunctionN> func) {
        return (evaluator, a) -> new ValTransit(
                List.of(a),
                func.get(),
                evaluator
        );
    }

}