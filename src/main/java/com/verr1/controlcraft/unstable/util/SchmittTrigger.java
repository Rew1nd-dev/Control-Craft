package com.verr1.controlcraft.unstable.util;

public class SchmittTrigger {




    private double min;



    private double max;

    private boolean state;

    public SchmittTrigger(double min, double max) {
        this.min = min;
        this.max = max;
    }

    public boolean update(double value) {
        if (value > max) {
            state = true;
        } else if (value < min) {
            state = false;
        }
        return state;
    }

    public boolean peek(){
        return state;
    }

    public double min() {
        return min;
    }
    public double max() {
        return max;
    }

    public void setMax(double max) {
        this.max = max;
    }
    public void setMin(double min) {
        this.min = min;
    }
}
