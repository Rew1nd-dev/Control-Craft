package com.verr1.controlcraft.foundation.cimulink.core.components.digital;


import com.verr1.controlcraft.foundation.cimulink.core.api.StateFactory;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.Temporal;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import kotlin.Pair;

import java.util.List;

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
