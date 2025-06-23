package com.verr1.controlcraft.foundation.cimulink.game.peripheral;

import com.verr1.controlcraft.content.blocks.flap.CompactFlapBlockEntity;

public class FlapPlant extends Plant {

    private final CompactFlapBlockEntity cfb;


    public FlapPlant(CompactFlapBlockEntity cfb) {
        super(
                new builder().in("angle", cfb::setAttackAngle)
        );
        this.cfb = cfb;
    }



    private CompactFlapBlockEntity plant() {
        return cfb;
    }


}
