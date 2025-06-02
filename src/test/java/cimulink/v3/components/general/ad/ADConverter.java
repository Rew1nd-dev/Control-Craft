package cimulink.v3.components.general.ad;

import cimulink.v3.components.general.Combinational;
import cimulink.v3.utils.ArrayUtils;

import java.util.List;

public class ADConverter extends Combinational {

    private double threshold;

    public ADConverter(int n, double threshold) {
        super(
                ArrayUtils.createInputNames(n),
                ArrayUtils.createOutputNames(n)
        );
        this.threshold = threshold;
    }

    public double threshold() {
        return threshold;
    }

    public void setThreshold(double threshold) {
        this.threshold = threshold;
    }

    @Override
    protected List<Double> transform(List<Double> inputs) {
        return ArrayUtils.mapToList(
                inputs,
                d -> d < threshold ? 0.0: 1.0
        );
    }
}
