package com.verr1.controlcraft.foundation.cimulink.game.peripheral;

import com.simibubi.create.content.kinetics.speedController.SpeedControllerBlockEntity;
import com.verr1.controlcraft.unstable.blocks.AiCannonBaseBlockEntity;

public class AiCannonPlant extends MainThreadPlant<AiCannonBaseBlockEntity>{

    public AiCannonPlant(AiCannonBaseBlockEntity cannon) {
        super(new builder()
                .in("fire", s -> schedule(cannon, s, AiCannonPlant::accept)),
                cannon
        );
    }

    private static void accept(AiCannonBaseBlockEntity sp, double t){
        if(t < 0.5)return;
        sp.fire();
    }


}
