package cimulink.factory.preset;

import kotlin.Pair;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.function.Function;

public class Component11<S> extends ComponentNM<S>{
    private static final String INPUT_A = "a";
    private static final String OUTPUT_O = "o";

    public Component11(
            Function<Pair<@NotNull List<Double>, @NotNull S>, Pair<@NotNull List<Double>, @NotNull S>> transition,
            S defaultState
    ) {
        super(List.of(INPUT_A), List.of(OUTPUT_O), transition, defaultState);
    }

    public Component11(Function<List<Double>, List<Double>> transform) {
        super(List.of(INPUT_A), List.of(OUTPUT_O), transform);
    }


    public String a(){
        return INPUT_A;
    }

    public String o(){
        return OUTPUT_O;
    }

}
