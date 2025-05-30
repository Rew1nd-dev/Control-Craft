package cimulink.v1.factory.basic.analog;

import cimulink.v1.Component;
import cimulink.v1.NamedComponent;
import cimulink.v1.components.CombinationalComponent;
import cimulink.v1.components.TemporalComponent;
import kotlin.Pair;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.function.Function;

public class AnalogNM<S> extends NamedComponent {

    public AnalogNM(
            List<String> inputs,
            List<String> outputs,
            Function<Pair<@NotNull List<Double>, @NotNull S>, Pair<@NotNull List<Double>, @NotNull S>> transition,
            S defaultState
    ) {
        super(temporal(inputs.size(), outputs.size(), transition, defaultState), inputs, outputs);
    }

    public AnalogNM(
            List<String> inputs,
            List<String> outputs,
            Function<List<Double>, List<Double>> transform
    ) {
        super(combinational(inputs.size(), outputs.size(), transform), inputs, outputs);
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

    /*

    new Component(false, N, M) {
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
    * Component(false, N, M) {
            private T state = defaultState;
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
    * */
}
