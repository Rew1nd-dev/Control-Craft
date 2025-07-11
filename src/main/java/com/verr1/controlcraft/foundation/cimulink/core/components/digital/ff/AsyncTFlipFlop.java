package com.verr1.controlcraft.foundation.cimulink.core.components.digital.ff;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.digital.BooleanTemporal;
import com.verr1.controlcraft.foundation.cimulink.core.registry.CimulinkFactory;
import com.verr1.controlcraft.foundation.cimulink.core.registry.Factory;
import kotlin.Pair;

import java.util.List;

public class AsyncTFlipFlop extends BooleanTemporal<AsyncTFlipFlop.TFFState> {


    public AsyncTFlipFlop() {
        super(List.of("i", "clk"), List.of("o"), TFFState::new);
    }

    @Override
    protected Pair<List<Boolean>, TFFState> transitBoolean(List<Boolean> input, TFFState state) {
        boolean i = input.get(0);
        boolean clk = input.get(1);
        boolean lastOutput = state.lastOutput;
        boolean lastClock = state.lastClock;

        if (clk && !lastClock) { // Rising edge
            lastOutput = !i; // Toggle sampled input
        }

        state.lastOutput = lastOutput;
        state.lastClock = clk;

        return new Pair<>(List.of(lastOutput), state);
    }


    public static class TFFState {
        boolean lastOutput = false;
        boolean lastClock = false;
    }

    @Override
    public Factory<? extends NamedComponent> factory() {
        return CimulinkFactory.ASYNC_T_FF;
    }
}
