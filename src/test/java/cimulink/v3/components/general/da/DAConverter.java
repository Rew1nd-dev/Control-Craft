package cimulink.v3.components.general.da;

import cimulink.v3.components.general.Combinational;
import cimulink.v3.utils.ArrayUtils;

import java.util.List;

public class DAConverter extends Combinational {

    private double min;
    private double max;

    public DAConverter(int n, double min, double max) {
        super(
                ArrayUtils.createInputNames(n),
                ArrayUtils.createOutputNames(n)
        );
        this.min = min;
        this.max = max;
    }

    @Override
    protected List<Double> transform(List<Double> inputs) {
        return ArrayUtils.mapToList(
                inputs,
                d -> min + (max - min) * d
        );
    }
}
