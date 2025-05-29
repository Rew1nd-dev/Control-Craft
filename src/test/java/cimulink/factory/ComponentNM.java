package cimulink.factory;

import cimulink.Component;
import cimulink.NamedComponent;
import kotlin.Pair;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

public class ComponentNM<S> extends NamedComponent {



    ComponentNM(
            List<String> inputs,
            List<String> outputs,
            Function<Pair<@NotNull List<Double>, @NotNull S>, Pair<@NotNull List<Double>, @NotNull S>> transition,
            boolean temporal
    ) {
        super(temporal(inputs.size(), outputs.size(), transition), inputs, outputs);
    }

    ComponentNM(
            List<String> inputs,
            List<String> outputs,
            Function<List<Double>, List<Double>> transform
    ) {
        super(combinational(inputs.size(), outputs.size(), transform), List.of("a", "b"), List.of("o"));
    }


    public static ComponentNM<Void> ofCombinational(
            List<String> inputs,
            List<String> outputs,
            Function<List<Double>, List<Double>> transform
    ){
        return new ComponentNM<>(inputs, outputs, transform);
    }

    public static<S> ComponentNM<S> ofTemporal(
            List<String> inputs,
            List<String> outputs,
            Function<Pair<@NotNull List<Double>, @NotNull S>, Pair<@NotNull List<Double>, @NotNull S>> transition
    ){
        return new ComponentNM<>(inputs, outputs, transition, true);
    }

    private static <T> Component combinational(
            int N,
            int M,
            Function<List<Double>, List<Double>> transform
    ){
        return new Component(false, N, M) {
            private final List<Double> cachedOutput = new ArrayList<>(M);
            private final List<Double> view = Collections.unmodifiableList(cachedOutput);

            @Override
            public @NotNull List<Double> supply() {
                return view;
            }

            @Override
            public void consume(@NotNull List<Double> inputs) {
                if(inputs.size() != N){
                    throw new IllegalArgumentException("Input size mismatch, expect: " + N + ", got: " + inputs.size());
                }
                cachedOutput.clear();
                cachedOutput.addAll(transform.apply(inputs));

            }
        };
    }

    private static<T> Component temporal(
            int N,
            int M,
            Function<
                    Pair<@NotNull List<Double>, @NotNull T>,
                    Pair<@NotNull List<Double>, @NotNull T>
            > transition
    ){
        return new Component(false, N, M) {
            private T state;
            private final List<Double> cachedOutput = new ArrayList<>(M);
            private final List<Double> view = Collections.unmodifiableList(cachedOutput);

            @Override
            public @NotNull List<Double> supply() {
                return view;
            }

            @Override
            public void consume(@NotNull List<Double> inputs) {
                if(inputs.size() != N){
                    throw new IllegalArgumentException("Input size mismatch, expect: " + N + ", got: " + inputs.size());
                }
                cachedOutput.clear();
                var o = transition.apply(new Pair<>(inputs, state));
                cachedOutput.addAll(o.getFirst());
                state = o.getSecond();
            }
        };
    }
}
