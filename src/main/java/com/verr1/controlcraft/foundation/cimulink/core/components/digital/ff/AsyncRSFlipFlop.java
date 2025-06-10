package com.verr1.controlcraft.foundation.cimulink.core.components.digital.ff;

import com.verr1.controlcraft.foundation.cimulink.core.api.StateFactory;
import com.verr1.controlcraft.foundation.cimulink.core.components.digital.BooleanTemporal;
import kotlin.Pair;

import java.util.List;

public class AsyncRSFlipFlop extends BooleanTemporal<Boolean> {

    public AsyncRSFlipFlop() {
        super(List.of("R", "S", "clk"), List.of("Q"), () -> false);

    }

    @Override
    protected Pair<List<Boolean>, Boolean> transitBoolean(List<Boolean> input, Boolean lastClock) {
        boolean R = input.get(0);
        boolean S = input.get(1);
        boolean clk = input.get(2);

        boolean out = false;
        if (R && !S) {
            out = false; // Reset
        } else if (!R && S) {
            out = true; // Set
        } else if (R && S) {
            out = lastClock; // Invalid state, maintain last state
        } else {
            out = lastClock; // Maintain last state
        }
        return new Pair<>(List.of(out), clk);
    }
}
