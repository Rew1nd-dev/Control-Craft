package com.verr1.controlcraft.foundation.cimulink.core.components.digital.ff;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.digital.BooleanTemporal;
import com.verr1.controlcraft.foundation.cimulink.core.registry.CimulinkFactory;
import com.verr1.controlcraft.foundation.cimulink.core.registry.Factory;
import kotlin.Pair;

import java.util.List;

public class AsyncJKFlipFlop extends BooleanTemporal<AsyncJKFlipFlop.JKFFState> {


    public AsyncJKFlipFlop() {
        super(List.of("J", "K", "clk"), List.of("Q", "Qb"), JKFFState::new);
    }

    @Override
    protected Pair<List<Boolean>, JKFFState> transitBoolean(List<Boolean> input, JKFFState state) {
        boolean j = input.get(0);
        boolean k = input.get(1);
        boolean clk = input.get(2);
        boolean lastOutput = state.lastOutput;
        boolean lastClock = state.lastClock;

        if (clk && !lastClock) { // Rising edge
            if (j && !k) {
                lastOutput = true; // Set
            } else if (!j && k) {
                lastOutput = false; // Reset
            } else if (j && k) {
                lastOutput = !lastOutput; // Toggle
            }
        }

        state.lastOutput = lastOutput;
        state.lastClock = clk;

        return new Pair<>(List.of(lastOutput, !lastOutput), state);
    }

    public static class JKFFState {
        boolean lastOutput = false;
        boolean lastClock = false;
    }

    @Override
    public Factory<? extends NamedComponent> factory() {
        return CimulinkFactory.ASYNC_JK_FF;
    }
}
