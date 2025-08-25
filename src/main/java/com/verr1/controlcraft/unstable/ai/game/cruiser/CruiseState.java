package com.verr1.controlcraft.unstable.ai.game.cruiser;

import com.verr1.controlcraft.utils.MathUtils;

public class CruiseState {
    double yVariant = 0;
    double latestY = 0;
    double range = 60;

    public CruiseState newY(double y){
        if(Math.abs(y - latestY) < 10){
            yVariant = MathUtils.clamp(y - latestY + yVariant, -range, range);
        }
        latestY = y;
        return this;
    }

    public double cruiseRatio(){
        return Math.sqrt((1.001 - 0.6 * yVariant / range));
    }
}
