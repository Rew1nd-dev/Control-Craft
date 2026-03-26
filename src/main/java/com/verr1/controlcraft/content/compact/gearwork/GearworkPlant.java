package com.verr1.controlcraft.content.compact.gearwork;

import com.verr1.controlcraft.foundation.cimulink.game.peripheral.MutablePlant;
import com.verr1.gearwork.content.landinggear.LandingGearBlockEntity;

public class GearworkPlant extends MutablePlant {

    private boolean lastTriggerGearUp = false;


    public GearworkPlant(LandingGearBlockEntity gear) {
        super(new builder()
            .in("steer", (s, v) -> gear.setTargetSteer(v.floatValue()))
            .in("brake", (s, v) -> gear.setFreespin(v < 0.5))
            .in("fold", (s, v) -> {
                GearworkPlant self = ((GearworkPlant) s);
                int trig = self.trigger(v);
                boolean folded = gear.isRetracted();
                if(trig == 1){
                    gear.requestFoldState(!folded, v > 0 ? 1 : -1);
                }
            })
        );
    }

    public int trigger(double v){
        boolean trig = Math.abs(v) > 0.5;
        int res = 0;
        // posEdge
        if(lastTriggerGearUp != trig && trig){
            res = 1;
        }
        // negEdge
        if(lastTriggerGearUp != trig && !trig){
            res = -1;
        }
        lastTriggerGearUp = trig;
        return res;
    }

}
