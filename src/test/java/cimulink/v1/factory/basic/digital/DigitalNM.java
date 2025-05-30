package cimulink.v1.factory.basic.digital;

import cimulink.v1.Component;
import cimulink.v1.NamedComponent;
import cimulink.v1.components.CombinationalComponent;
import cimulink.v1.components.TemporalComponent;
import kotlin.Pair;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.function.Function;

public class DigitalNM<S> extends NamedComponent {

    public static final double TRUE = 1.0;
    public static final double FALSE = 0.0;

    public DigitalNM(
            List<String> inputs,
            List<String> outputs,
            Function<Pair<@NotNull List<Boolean>, @NotNull S>, Pair<@NotNull List<Boolean>, @NotNull S>> transition,
            S defaultState
    ) {
        super(temporal(inputs.size(), outputs.size(), wrapTemporal(transition), defaultState), inputs, outputs);
    }

    public DigitalNM(
            List<String> inputs,
            List<String> outputs,
            Function<List<Boolean>, List<Boolean>> transform
    ) {
        super(combinational(inputs.size(), outputs.size(), wrapCombinational(transform)), inputs, outputs);
    }

    public static<S> Function<Pair<List<Double>, S>, Pair<List<Double>, S>> wrapTemporal(Function<Pair<List<Boolean>, S>, Pair<List<Boolean>, S>> booleanFunction){
        return pair -> {
            List<Boolean> inputBooleans = pair.getFirst().stream().map(DigitalNM::unwrap).toList();
            Pair<List<Boolean>, S> booleanPair = new Pair<>(inputBooleans, pair.getSecond());
            Pair<List<Boolean>, S> result = booleanFunction.apply(booleanPair);
            List<Double> outputDoubles = result.getFirst().stream().map(DigitalNM::wrap).toList();
            return new Pair<>(outputDoubles, result.getSecond());
        };
    }

    public static Function<List<Double>, List<Double>> wrapCombinational(Function<List<Boolean>, List<Boolean>> booleanFunction){
        return doubles -> {
            List<Boolean> inputBooleans = doubles.stream().map(DigitalNM::unwrap).toList();
            List<Boolean> outputBooleans = booleanFunction.apply(inputBooleans);
            return outputBooleans.stream().map(DigitalNM::wrap).toList();
        };
    }

    public static double wrap(boolean value){
        return value ? 1.0 : 0.0;
    }

    public static boolean unwrap(double value){
        return value > 0.5;
    }

    private static <T> Component combinational(
            int N,
            int M,
            Function<List<Double>, List<Double>> transform
    ){
        return new CombinationalComponent(N, M, transform);
    }

    private static<T> Component temporal(
            int N,
            int M,
            Function<
                    Pair<@NotNull List<Double>, @NotNull T>,
                    Pair<@NotNull List<Double>, @NotNull T>
                    > transition,
            T defaultState
    ){
        return new TemporalComponent<T>(N, M, transition, defaultState);
    }
}
