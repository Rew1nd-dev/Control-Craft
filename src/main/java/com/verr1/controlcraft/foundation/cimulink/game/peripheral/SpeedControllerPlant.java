package com.verr1.controlcraft.foundation.cimulink.game.peripheral;

import com.simibubi.create.content.kinetics.speedController.SpeedControllerBlockEntity;

public class SpeedControllerPlant extends MainThreadPlant<SpeedControllerBlockEntity> {



    public SpeedControllerPlant(SpeedControllerBlockEntity sp) {
        super(
                new builder()
                .in("target", s -> schedule(sp, s, SpeedControllerPlant::accept))
                .out("speed", () -> (double)sp.getSpeed()),
                sp
        );
    }

    private static void accept(SpeedControllerBlockEntity sp, double t){
        sp.targetSpeed.setValue((int)t);
    }

}
