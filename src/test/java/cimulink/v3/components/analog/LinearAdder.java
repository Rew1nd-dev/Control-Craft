package cimulink.v3.components.analog;

import cimulink.v3.components.general.Combinational;
import cimulink.v3.utils.ArrayUtils;

import java.util.List;

public class LinearAdder extends Combinational {

    private final List<Double> coefficients;

    public LinearAdder(List<Double> coefficients) {
        super(
                ArrayUtils.createInputNames(coefficients.size()),
                ArrayUtils.SINGLE_OUTPUT
        );
        this.coefficients = coefficients;
    }



    @Override
    protected List<Double> transform(List<Double> inputs) {
        ArrayUtils.AssertSize(inputs, n());
        double result = 0;
        for (int i = 0; i < n(); i++) {
            result += inputs.get(i) * coefficients.get(i);
        }
        return List.of(result);
    }
}
