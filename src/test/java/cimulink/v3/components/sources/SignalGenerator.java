package cimulink.v3.components.sources;

import cimulink.v3.api.StateFactory;
import cimulink.v3.components.general.Temporal;
import kotlin.Pair;

import java.util.List;
import java.util.function.Supplier;
import java.util.stream.IntStream;

public abstract class SignalGenerator<S> extends Temporal<S> {


    public SignalGenerator(
            StateFactory<S> defaultState
    ) {
        super(List.of("@signal"), List.of("signal"), defaultState);
    }

    @Override
    protected Pair<List<Double>, S> transit(List<Double> input, S state) {
        return new Pair<>(List.of(generate(state)), next(state));
    }

    /*
    * @Override
    public List<Integer> changedOutput() {
        return IntStream.range(0, n()).boxed().toList();
    }

    @Override
    public boolean anyOutputChanged() {
        return true;
    }

    @Override
    public boolean outputChanged(int index) {
        return true;
    }
    * no need to override these, since onPositiveEdge() will call transit and output will change normally, should take than 1 cycle to take effect
    *
    * */

    protected abstract double generate(S state);

    protected abstract S next(S current);

}
