package com.verr1.controlcraft.foundation.cimulink.core.components.digital.ff;

import com.verr1.controlcraft.foundation.cimulink.core.components.digital.BooleanTemporal;
import kotlin.Pair;

import java.util.List;

public class AsyncDFlipFlop extends BooleanTemporal<AsyncDFlipFlop.DFFState> {

    public AsyncDFlipFlop() {
        super(List.of("i", "clk"), List.of("o"), DFFState::new);
    }

    @Override
    protected Pair<List<Boolean>, DFFState> transitBoolean(List<Boolean> input, DFFState state) {
        boolean i = input.get(0);
        boolean clk = input.get(1);
        boolean lastOutput = state.lastOutput;
        boolean lastClock = state.lastClock;

        if (clk && !lastClock) { // Rising edge
            lastOutput = i; // set sampled input
        }

        state.lastOutput = lastOutput;
        state.lastClock = clk;

        return new Pair<>(List.of(lastOutput), state);
    }


    public static class DFFState {
        boolean lastOutput = false;
        boolean lastClock = false;
    }

}
