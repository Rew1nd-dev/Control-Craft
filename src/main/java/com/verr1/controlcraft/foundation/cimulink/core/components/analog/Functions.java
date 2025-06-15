package com.verr1.controlcraft.foundation.cimulink.core.components.analog;



import com.verr1.controlcraft.foundation.cimulink.core.components.general.Combinational;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.MathUtils;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.nbt.CompoundTag;

import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

public class Functions {



    public static final Function<Integer, FunctionN> PRODUCT = n -> new FunctionN(n) {
        @Override
        protected List<Double> transform(List<Double> inputs) {
            return List.of(inputs.stream().reduce(1.0, (a, b) -> a * b));
        }
    };

    public static final Function<Integer, FunctionN> MAX = n -> new FunctionN(n) {
        @Override
        protected List<Double> transform(List<Double> inputs) {
            return List.of(inputs.stream().max(Double::compareTo).orElse(0.0));
        }
    };

    public static final Function<Integer, FunctionN> MIN = n -> new FunctionN(n) {
        @Override
        protected List<Double> transform(List<Double> inputs) {
            return List.of(inputs.stream().min(Double::compareTo).orElse(0.0));
        }
    };

    public static final Supplier<FunctionN> ANGLE_FIX = () -> new FunctionN(1) {
        @Override
        protected List<Double> transform(List<Double> inputs) {
            return List.of(MathUtils.radErrFix(inputs.get(0)));
        }
    };

    public static final Supplier<FunctionN> POWER = () -> new FunctionN(2) {
        @Override
        protected List<Double> transform(List<Double> inputs) {
            double base = inputs.get(0);
            double exponent = inputs.get(1);
            int floorExponent = (int) Math.floor(exponent);
            if(Math.abs(exponent - floorExponent) < 1e-10) {
                // If exponent is an integer, use Math.pow
                boolean odd = (floorExponent % 2) != 0;
                double sign = odd ? Math.signum(base) : 1.0;
                return List.of(sign * Math.pow(Math.abs(base), floorExponent));
            }else {
                double sign = Math.signum(base);
                return List.of(sign * Math.pow(Math.abs(base), exponent));
            }


        }
    };

    public static final Supplier<FunctionN> SIN = () -> new FunctionN(1) {
        @Override
        protected List<Double> transform(List<Double> inputs) {
            return List.of(Math.sin(inputs.get(0)));
        }
    };

    public static final Supplier<FunctionN> COS = () -> new FunctionN(1) {
        @Override
        protected List<Double> transform(List<Double> inputs) {
            return List.of(Math.cos(inputs.get(0)));
        }
    };

    public static final Supplier<FunctionN> TAN = () -> new FunctionN(1) {
        @Override
        protected List<Double> transform(List<Double> inputs) {
            return List.of(Math.tan(inputs.get(0)));
        }
    };

    public static int deserializeN(CompoundTag tag){
        return SerializeUtils.INT.deserialize(tag.getCompound("n"));
    }


    public static abstract class FunctionN extends Combinational {
        private final int n;

        public FunctionN(int n) {
            super(ArrayUtils.createInputNames(n), ArrayUtils.SINGLE_OUTPUT);
            this.n = n;
        }

        public CompoundTag serialize(){
            return CompoundTagBuilder.create()
                    .withCompound("n", SerializeUtils.INT.serialize(n))
                    .build();
        }


        // Functions Constants like PRODUCT, MIN, MAX are actually deserializers

    }

}
