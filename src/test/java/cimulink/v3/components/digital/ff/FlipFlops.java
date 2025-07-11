package cimulink.v3.components.digital.ff;

import cimulink.v3.api.StateFactory;
import cimulink.v3.components.general.Temporal;
import cimulink.v3.components.digital.BooleanTemporal;
import cimulink.v3.utils.ArrayUtils;
import kotlin.Pair;

import java.util.List;
import java.util.function.Supplier;

public class FlipFlops {

    public static final Supplier<Temporal<Boolean>> RS_FF = () -> new FF11<>(() -> false){
        @Override
        protected Pair<Boolean, Boolean> transit(Boolean input, Boolean state) {
            if (input) {
                return new Pair<>(true, false); // Set
            } else {
                return new Pair<>(false, true); // Reset
            }
        }
    };

    public static final Supplier<Temporal<Boolean>> D_FF = () -> new FF11<>(() -> false){
        @Override
        protected Pair<Boolean, Boolean> transit(Boolean input, Boolean state) {
            return new Pair<>(input, false);
        }
    };

    public static final Supplier<Temporal<Boolean>> T_FF = () -> new FF11<>(() -> false){
        @Override
        protected Pair<Boolean, Boolean> transit(Boolean input, Boolean state) {
            return new Pair<>(!state, false);
        }
    };

    public static final Supplier<Temporal<Boolean>> JK_FF = () -> new BooleanTemporal<>(
            List.of("J", "K"),
            List.of("Q", "Qb"),
            () -> false
    ) {
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
