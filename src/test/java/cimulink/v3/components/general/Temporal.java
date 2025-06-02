package cimulink.v3.components.general;

import cimulink.v3.api.StateFactory;
import cimulink.v3.components.NamedComponent;
import kotlin.Pair;

import java.util.List;
import java.util.function.Supplier;

public abstract class Temporal<S> extends NamedComponent {

    private S state;
    private final Supplier<S> stateGetter;

    public Temporal(
            List<String> inputs,
            List<String> outputs,
            StateFactory<S> stateGetter
    ) {
        super(inputs, outputs);
        this.stateGetter = stateGetter;
        state = stateGetter.get();
    }

    @Override
    public void onInputChange(Integer... inputIndexes) {

    }

    @Override
    public List<Integer> propagateTo(int inputIndex) {
        return List.of();
    }

    @Override
    public final void onPositiveEdge() {
        var o = transit(retrieveInput(), state);
        state = o.getSecond();
        updateOutput(o.getFirst());
    }

    @Override
    public void reset() {
        state = stateGetter.get();
    }

    protected abstract Pair<List<Double>, S> transit(List<Double> input, S state);


}
