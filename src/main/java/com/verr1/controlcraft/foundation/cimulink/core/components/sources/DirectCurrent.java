package com.verr1.controlcraft.foundation.cimulink.core.components.sources;

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
