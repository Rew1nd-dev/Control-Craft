package com.verr1.controlcraft.foundation.cimulink.core.components.digital.gates;



import com.verr1.controlcraft.foundation.cimulink.core.components.digital.BooleanCombinational;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;

import java.util.List;
import java.util.function.Function;

public class Gates {


    public static final Function<Integer, BooleanCombinational> AND = n -> new Gate(n) {
        @Override
        protected Boolean transform1(List<Boolean> inputs) {
            return inputs.stream().allMatch(b -> b);
        }
    };

    public static final Function<Integer, BooleanCombinational> OR = n -> new Gate(n) {
        @Override
        protected Boolean transform1(List<Boolean> inputs) {
            return inputs.stream().anyMatch(b -> b);
        }
    };

    public static final Function<Integer, BooleanCombinational> XOR = n -> new Gate(n) {
        @Override
        protected Boolean transform1(List<Boolean> inputs) {
            return inputs.stream().filter(b -> b).count() % 2 == 1;
        }
    };

    public static final Function<Integer, BooleanCombinational> NOT = n -> new Gate(1) {
        @Override
        protected Boolean transform1(List<Boolean> inputs) {
            return !inputs.get(0);
        }
    };

    public static final Function<Integer, BooleanCombinational> NAND = n -> new Gate(n) {
        @Override
        protected Boolean transform1(List<Boolean> inputs) {
            return !inputs.stream().allMatch(b -> b);
        }
    };



    public static abstract class Gate extends BooleanCombinational{

        public Gate(int n) {
            super(
                    ArrayUtils.createInputNames(n),
                    ArrayUtils.SINGLE_OUTPUT
            );
        }

        @Override
        protected final List<Boolean> transformBoolean(List<Boolean> inputs) {
            return List.of(transform1(inputs));
        }


        protected abstract Boolean transform1(List<Boolean> inputs);
    }
}
