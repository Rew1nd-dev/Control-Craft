package com.verr1.controlcraft.foundation.cimulink.core.components.digital.ff;


import com.verr1.controlcraft.foundation.cimulink.core.api.StateFactory;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.digital.BooleanTemporal;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.Temporal;
import com.verr1.controlcraft.foundation.cimulink.core.registry.CimulinkFactory;
import com.verr1.controlcraft.foundation.cimulink.core.registry.Factory;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import kotlin.Pair;

import java.util.List;
import java.util.function.Supplier;

public class FlipFlops {

    public static final Supplier<Temporal<Boolean>> RS_FF = () -> new BooleanTemporal<>(
            List.of("R", "S"),
            List.of("Q", "Qb"),
            () -> false
    ) {
        private final String ID = "RS";
        @Override
        protected Pair<List<Boolean>, Boolean> transitBoolean(List<Boolean> input, Boolean state) {
            boolean r = input.get(0);
            boolean s = input.get(1);
            boolean nextState = state;

            if (r && !s) {
                nextState = false; // Reset
            } else if (!r && s) {
                nextState = true; // Set
            } else if (r && s) {
                nextState = false; // Invalid state, reset
            }

            return new Pair<>(List.of(nextState, !nextState), nextState);
        }

        @Override
        public Factory<? extends NamedComponent> factory() {
            return CimulinkFactory.RS_FF;
        }
    };

    public static final Supplier<Temporal<Boolean>> D_FF = () -> new FF11<>(() -> false){
        private final String ID = "D";
        @Override
        protected Pair<Boolean, Boolean> transit(Boolean input, Boolean state) {
            return new Pair<>(input, false);
        }
        @Override
        public Factory<? extends NamedComponent> factory() {
            return CimulinkFactory.D_FF;
        }
    };

    public static final Supplier<Temporal<Boolean>> T_FF = () -> new FF11<>(() -> false){
        private final String ID = "T";
        @Override
        protected Pair<Boolean, Boolean> transit(Boolean input, Boolean state) {
            return new Pair<>(!input, false);
        }
        @Override
        public Factory<? extends NamedComponent> factory() {
            return CimulinkFactory.T_FF;
        }
    };

    public static final Supplier<Temporal<Boolean>> JK_FF = () -> new BooleanTemporal<>(
            List.of("J", "K"),
            List.of("Q", "Qb"),
            () -> false
    ) {
        private final String ID = "JK";
        @Override
        protected Pair<List<Boolean>, Boolean> transitBoolean(List<Boolean> input, Boolean state) {
            boolean j = input.get(0);
            boolean k = input.get(1);
            boolean nextState = state;

            if (j && !k) {
                nextState = true; // Set
            } else if (!j && k) {
                nextState = false; // Reset
            } else if (j && k) {
                nextState = !state; // Toggle
            }

            return new Pair<>(List.of(nextState, !nextState), nextState);
        }
        @Override
        public Factory<? extends NamedComponent> factory() {
            return CimulinkFactory.JK_FF;
        }
    };



    public static abstract class FF11<S> extends BooleanTemporal<S>{

        public FF11(StateFactory<S> defaultState) {
            super(ArrayUtils.SINGLE_INPUT, ArrayUtils.SINGLE_OUTPUT, defaultState);
        }

        @Override
        protected Pair<List<Boolean>, S> transitBoolean(List<Boolean> input, S state) {
            var o = transit(input.get(0), state);
            return new Pair<>(List.of(o.getFirst()), o.getSecond());
        }

        protected abstract Pair<Boolean, S> transit(Boolean input, S state);
    }

}
