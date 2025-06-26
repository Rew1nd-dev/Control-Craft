package com.verr1.controlcraft.foundation.cimulink.game.peripheral;

import com.verr1.controlcraft.content.blocks.flap.CompactFlapBlockEntity;
import com.verr1.controlcraft.content.blocks.flap.FlapBearingBlockEntity;

public class FlapBearingPlant extends Plant{

    private final FlapBearingBlockEntity cfb;


    public FlapBearingPlant(FlapBearingBlockEntity cfb) {
        super(
                new builder().in("angle", cfb::setAngle)
        );
        this.cfb = cfb;
    }



    private FlapBearingBlockEntity plant() {
        return cfb;
    }
}
