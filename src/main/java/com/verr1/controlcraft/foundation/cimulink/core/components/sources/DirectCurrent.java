package com.verr1.controlcraft.foundation.cimulink.core.components.sources;

public class DirectCurrent extends SignalGenerator<Double> {

    private double dc = 0;

    public DirectCurrent(double dc) {
        super(() -> dc);
        this.dc = dc;
    }

    public void setDc(double dc){
        this.dc = dc;
    }

    @Override
    protected double generate(Double dc) {
        return this.dc;
    }

    @Override
    protected Double next(Double $) {
        return this.dc;
    }
}
