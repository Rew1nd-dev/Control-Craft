package cimulink.v3.components.sources;

import cimulink.v3.api.StateFactory;
import cimulink.v3.components.general.Temporal;
import kotlin.Pair;

import java.util.List;
import java.util.function.Supplier;

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

    protected abstract double generate(S state);

    protected abstract S next(S current);

}
