package cimulink.v3.components.digital;

import cimulink.v3.components.general.Combinational;
import cimulink.v3.utils.ArrayUtils;

import java.util.List;

public abstract class BooleanCombinational extends Combinational {



    public BooleanCombinational(List<String> inputs, List<String> outputs) {
        super(inputs, outputs);
    }

    @Override
    protected final List<Double> transform(List<Double> inputs) {
        return ArrayUtils.mapToList(
                transformBoolean(
                        ArrayUtils.mapToList(inputs, d -> d > 0.5)
                ),
                b -> b ? 1.0 : 0.0
        );
    }



    protected abstract List<Boolean> transformBoolean(List<Boolean> inputs);

}
