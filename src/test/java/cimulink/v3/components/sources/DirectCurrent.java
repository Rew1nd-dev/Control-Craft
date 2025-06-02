package cimulink.v3.components.sources;

import cimulink.v3.components.general.Combinational;

import java.util.List;

public class DirectCurrent extends SignalGenerator<Double> {


    public DirectCurrent(double dc) {
        super(() -> dc);
    }

    @Override
    protected double generate(Double dc) {
        return dc;
    }

    @Override
    protected Double next(Double dc) {
        return dc;
    }
}
