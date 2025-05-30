package cimulink.v2.components.compiled;

import cimulink.v2.Component;
import kotlin.Pair;

import java.util.List;

public abstract class Temporal<S> extends Component {

    private S state;

    public Temporal(int n, int m, S defaultState) {
        super(n, m);
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

    protected abstract Pair<List<Double>, S> transit(List<Double> input, S state);


}
