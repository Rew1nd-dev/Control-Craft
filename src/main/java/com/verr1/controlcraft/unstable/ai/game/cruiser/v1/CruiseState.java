package com.verr1.controlcraft.unstable.ai.game.cruiser.v1;

import com.verr1.controlcraft.utils.MathUtils;

public class CruiseState {
    double yVariant = 0;
    double latestY = 0;
    double range = 20;

    public CruiseState newY(double y){
        yVariant = MathUtils.clamp(y - latestY + yVariant, -range, range);
        return this;
    }

    public double cruiseRatio(){
        return Math.sqrt((1.001 - 0.5 * yVariant / range));
    }
}
