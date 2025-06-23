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
        /*super(List.of(), List.of("lx", "ly", "rx", "ry", "lt", "rt"));     */

        this.lectern = lectern;
    }


    public TweakedLecternControllerBlockEntity plant(){
        return lectern;
    }

    /*
    * @Override
    public List<Integer> propagateTo(int inputIndex) {
        return List.of();
    }

    @Override
    public void onInputChange(Integer... inputIndexes) {

    }

    @Override
    public void onPositiveEdge() {
        updateOutput(List.of(
                (double)lectern.GetAxis(0),
                (double)lectern.GetAxis(1),
                (double)lectern.GetAxis(2),
                (double)lectern.GetAxis(3),
                (double)lectern.GetAxis(4),
                (double)lectern.GetAxis(5)
        ));
    }
    * */
}
