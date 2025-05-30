package cimulink.v1.factory.basic.analog;

import cimulink.v1.utils.ArrayUtils;
import kotlin.Pair;
import org.jetbrains.annotations.NotNull;

import java.util.function.Function;

public class Analog11<S> extends AnalogNM<S> {

    public Analog11(
            Function<Pair<Double, S>, Pair<Double, @NotNull S>> transition,
            S defaultState
    ) {
        super(ArrayUtils.SINGLE_INPUT, ArrayUtils.SINGLE_OUTPUT, ArrayUtils.wrapTemporal(transition), defaultState);
    }

    public Analog11(
            Function<Double, Double> transform
    ) {
        super(ArrayUtils.SINGLE_INPUT, ArrayUtils.SINGLE_OUTPUT, ArrayUtils.wrapCombinational(transform));
    }

    public String i(){
        return ArrayUtils.INPUT_I;
    }

    public String o(){
        return ArrayUtils.OUTPUT_O;
    }

}
