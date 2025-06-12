package com.verr1.controlcraft.foundation.cimulink.core.components.digital.gates;



import com.verr1.controlcraft.foundation.cimulink.core.components.digital.BooleanCombinational;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.nbt.CompoundTag;

import java.util.List;
import java.util.function.Function;

public class Gates {


    public static final Function<Integer, Gate> AND = n -> new Gate(n) {
        private final String ID = "AND";
        @Override
        protected Boolean transform1(List<Boolean> inputs) {
            return inputs.stream().allMatch(b -> b);
        }
    };

    public static final Function<Integer, Gate> OR = n -> new Gate(n) {
        private final String ID = "OR";
        @Override
        protected Boolean transform1(List<Boolean> inputs) {
            return inputs.stream().anyMatch(b -> b);
        }
    };

    public static final Function<Integer, Gate> XOR = n -> new Gate(n) {
        private final String ID = "XOR";
        @Override
        protected Boolean transform1(List<Boolean> inputs) {
            return inputs.stream().filter(b -> b).count() % 2 == 1;
        }
    };

    public static final Function<Integer, Gate> NOT = n -> new Gate(1) {
        private final String ID = "NOT";
        @Override
        protected Boolean transform1(List<Boolean> inputs) {
            return !inputs.get(0);
        }
    };

    public static final Function<Integer, Gate> NAND = n -> new Gate(n) {
        private final String ID = "NAND";
        @Override
        protected Boolean transform1(List<Boolean> inputs) {
            return !inputs.stream().allMatch(b -> b);
        }
    };

    public static int deserializeN(CompoundTag tag){
        return SerializeUtils.INT.deserialize(tag.getCompound("n"));
    }

    public static abstract class Gate extends BooleanCombinational{
        private final int n;

        public Gate(int n) {
            super(
                    ArrayUtils.createInputNames(n),
                    ArrayUtils.SINGLE_OUTPUT
            );
            this.n = n;
        }

        public CompoundTag serialize(){
            return CompoundTagBuilder.create()
                    .withCompound("n", SerializeUtils.INT.serialize(n))
                    .build();
        }

        @Override
        protected final List<Boolean> transformBoolean(List<Boolean> inputs) {
            return List.of(transform1(inputs));
        }


        protected abstract Boolean transform1(List<Boolean> inputs);
    }
}
