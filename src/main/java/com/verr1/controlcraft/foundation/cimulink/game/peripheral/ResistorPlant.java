package com.verr1.controlcraft.foundation.cimulink.game.peripheral;

import com.verr1.controlcraft.content.blocks.kinetic.resistor.KineticResistorBlockEntity;

public class ResistorPlant extends MainThreadPlant<KineticResistorBlockEntity>{

    public ResistorPlant(KineticResistorBlockEntity kbe) {
        super(
                new builder().in("ratio", t -> schedule(kbe, t, KineticResistorBlockEntity::setRatio)),
                kbe
        );

    }

}
