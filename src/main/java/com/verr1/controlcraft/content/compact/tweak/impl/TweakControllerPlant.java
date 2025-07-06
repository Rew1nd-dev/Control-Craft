package com.verr1.controlcraft.content.compact.tweak.impl;

import com.getitemfromblock.create_tweaked_controllers.block.TweakedLecternControllerBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.peripheral.Plant;

import java.util.List;

public class TweakControllerPlant extends Plant {

    private final TweakedLecternControllerBlockEntity lectern;

    public TweakControllerPlant(TweakedLecternControllerBlockEntity lectern) {
        super(new builder()
                .out("lx", () -> (double)lectern.GetAxis(0))
                .out("ly", () -> (double)lectern.GetAxis(1))
                .out("rx", () -> (double)lectern.GetAxis(2))
                .out("ry", () -> (double)lectern.GetAxis(3))
                .out("lt", () -> (double)lectern.GetAxis(4))
                .out("rt", () -> (double)lectern.GetAxis(5))
        );

        this.lectern = lectern;
    }


    public TweakedLecternControllerBlockEntity plant(){
        return lectern;
    }


}
