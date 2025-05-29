package cimulink.factory.basic.digital;

import cimulink.utils.ArrayUtils;
import kotlin.Pair;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.function.Function;

public class Digital11<S> extends DigitalNM<S>{

    public Digital11(
            Function<Pair<Boolean, S>, Pair<Boolean, S>> transition,
            S defaultState
    ) {
        super(ArrayUtils.SINGLE_INPUT, ArrayUtils.SINGLE_OUTPUT, ArrayUtils.wrapTemporal(transition), defaultState);
    }

    public Digital11(
            Function<Boolean, Boolean> transform
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
