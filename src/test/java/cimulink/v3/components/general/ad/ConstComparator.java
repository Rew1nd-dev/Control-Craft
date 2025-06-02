package cimulink.v3.components.general.ad;

import cimulink.v3.components.general.Combinational;

import java.util.List;

public class ConstComparator extends Combinational {

    private double threshold;

    public ConstComparator(double threshold) {
        super(List.of("i"), List.of("i>th", "i<th", "i=th"));
        this.threshold = threshold;
    }

    @Override
    protected List<Double> transform(List<Double> inputs) {
        double i = inputs.get(0);
        return List.of(
                i > threshold ? 1.0 : 0.0, // i > th
                i < threshold ? 1.0 : 0.0, // i < th
                Math.abs(i - threshold) < 1e-5 ? 1.0 : 0.0 // i = th
        );
    }
}
