package cimulink.factory.preset;

import kotlin.Pair;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.function.Function;

public class Component41<S> extends ComponentNM<S> {
    private static final String INPUT_A = "a";
    private static final String INPUT_B = "b";
    private static final String INPUT_C = "c";
    private static final String INPUT_D = "d";

    private static final String OUTPUT_O = "o";

    public Component41(
            Function<Pair<@NotNull List<Double>, @NotNull S>, Pair<@NotNull List<Double>, @NotNull S>> transition,
            S defaultState
    ) {
        super(List.of(INPUT_A, INPUT_B, INPUT_C, INPUT_D), List.of(OUTPUT_O), transition, defaultState);
    }

    public Component41(Function<List<Double>, List<Double>> transform) {
        super(List.of(INPUT_A, INPUT_B, INPUT_C, INPUT_D), List.of(OUTPUT_O), transform);
    }


    public String a(){
        return INPUT_A;
    }

    public String b(){
        return INPUT_B;
    }

    public String c(){
        return INPUT_C;
    }

    public String d(){
        return INPUT_D;
    }

    public String o(){
        return OUTPUT_O;
    }
}
