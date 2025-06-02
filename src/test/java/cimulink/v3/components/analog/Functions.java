package cimulink.v3.components.analog;

import cimulink.v3.components.general.Combinational;
import cimulink.v3.utils.ArrayUtils;

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





    public static abstract class FunctionN extends Combinational{

        public FunctionN(int n) {
            super(ArrayUtils.createInputNames(n), ArrayUtils.SINGLE_OUTPUT);
        }

    }

}
