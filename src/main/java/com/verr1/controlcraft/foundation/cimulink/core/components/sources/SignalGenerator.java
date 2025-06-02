package com.verr1.controlcraft.foundation.cimulink.core.components.sources;


import com.verr1.controlcraft.foundation.cimulink.core.api.StateFactory;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.Temporal;
import kotlin.Pair;

import java.util.List;

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
