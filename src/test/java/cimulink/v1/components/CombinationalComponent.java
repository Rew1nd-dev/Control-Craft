package cimulink.v1.components;

import cimulink.v1.Component;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

public class CombinationalComponent extends Component {

    private final List<Double> cachedOutput = new ArrayList<>();
    private final List<Double> cachedInput = new ArrayList<>();
    private final List<Double> view = Collections.unmodifiableList(cachedOutput);
    private final Function<List<Double>, List<Double>> transform;

    public CombinationalComponent(
            int N,
            int M,
            Function<List<Double>, List<Double>> transform
    ) {
        super(true, N, M);
        this.transform = transform;
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
        cachedOutput.clear();
        cachedOutput.addAll(transform.apply(cachedInput));
    }
}
