package com.verr1.controlcraft.foundation.cimulink.game.peripheral;

import com.verr1.controlcraft.content.blocks.flap.CompactFlapBlockEntity;

public class FlapPlant extends Plant {

    private final CompactFlapBlockEntity cfb;


    public FlapPlant(CompactFlapBlockEntity cfb) {
        super(
                new builder()
                    .in("angle", cfb::setAngle)
                    .in("tilt", cfb::setTilt)
                    .in("asm", v -> cfb.requestAssemble(v > 0.5))
        );
        this.cfb = cfb;
    }



    private CompactFlapBlockEntity plant() {
        return cfb;
    }


}
