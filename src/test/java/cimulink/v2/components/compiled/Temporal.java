package cimulink.v2.components.compiled;

import cimulink.v2.components.NamedComponent;
import kotlin.Pair;

import java.util.List;

public abstract class Temporal<S> extends NamedComponent {

    private S state;

    public Temporal(
            List<String> inputs,
            List<String> outputs,
            S defaultState
    ) {
        super(inputs, outputs);
        state = defaultState;
    }

    @Override
    public final void onInputChange() {}

    @Override
    public final void onPositiveEdge() {
        var o = transit(retrieveInput(), state);
        state = o.getSecond();
        updateOutput(o.getFirst());
    }

    @Override
    protected final boolean immediateInternal(int $) {
        return false;
    }

    protected abstract Pair<List<Double>, S> transit(List<Double> input, S state);


}
