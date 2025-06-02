package cimulink.v3.components.digital;

import cimulink.v3.api.StateFactory;
import cimulink.v3.components.general.Temporal;
import cimulink.v3.utils.ArrayUtils;
import kotlin.Pair;

import java.util.List;
import java.util.function.Supplier;

public abstract class BooleanTemporal<S> extends Temporal<S> {


    public BooleanTemporal(List<String> inputs, List<String> outputs, StateFactory<S> defaultState) {
        super(inputs, outputs, defaultState);
    }

    @Override
    protected final Pair<List<Double>, S> transit(List<Double> input, S state) {
        var o = transitBoolean(ArrayUtils.mapToList(input, d -> d > 0.5), state);
        return new Pair<>(
                ArrayUtils.mapToList(
                        o.getFirst(),
                        b -> b ? 1.0 : 0.0
                ),
                o.getSecond()
        );
    }

    protected abstract Pair<List<Boolean>, S> transitBoolean(List<Boolean> input, S state);

}
