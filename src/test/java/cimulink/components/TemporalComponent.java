package cimulink.components;

import cimulink.Component;
import kotlin.Pair;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

public class TemporalComponent<T> extends Component {

    private T state;
    private final List<Double> cachedOutput = new ArrayList<>();
    private final List<Double> cachedInput = new ArrayList<>();
    private final List<Double> view = Collections.unmodifiableList(cachedOutput);

    private final Function<Pair<@NotNull List<Double>, @NotNull T>, Pair<@NotNull List<Double>, @NotNull T>> transition;

    public TemporalComponent(
            int N,
            int M,
            Function<Pair<List<Double>, T>, Pair<List<Double>, T>> transition,
            T defaultState
    ) {
        super(false, N, M);
        this.state = defaultState;
        this.transition = transition;
    }

    @Override
    public @NotNull List<Double> supply() {
        return view;
    }

    @Override
    public void consume(@NotNull List<Double> inputs) {
        if(inputs.size() != N()){
            throw new IllegalArgumentException("Input size mismatch, expect: " + N() + ", got: " + inputs.size());
        }
        cachedInput.clear();
        cachedInput.addAll(inputs);
    }

    @Override
    public void transit() {
        var o = transition.apply(new Pair<>(cachedInput, state));
        cachedOutput.clear();
        cachedOutput.addAll(o.getFirst());
        state = o.getSecond();
    }
}
