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
                .out("b0" , () -> toDouble(lectern.GetButton(0 )))
                .out("b1" , () -> toDouble(lectern.GetButton(1 )))
                .out("b2" , () -> toDouble(lectern.GetButton(2 )))
                .out("b3" , () -> toDouble(lectern.GetButton(3 )))
                .out("b4" , () -> toDouble(lectern.GetButton(4 )))
                .out("b5" , () -> toDouble(lectern.GetButton(5 )))
                .out("b6" , () -> toDouble(lectern.GetButton(6 )))
                .out("b7" , () -> toDouble(lectern.GetButton(7 )))
                .out("b8" , () -> toDouble(lectern.GetButton(8 )))
                .out("b9" , () -> toDouble(lectern.GetButton(9 )))
                .out("b10", () -> toDouble(lectern.GetButton(10)))
                .out("b11", () -> toDouble(lectern.GetButton(11)))
                .out("b12", () -> toDouble(lectern.GetButton(12)))
                .out("b13", () -> toDouble(lectern.GetButton(13)))
                .out("b14", () -> toDouble(lectern.GetButton(14)))

        );
        /*super(List.of(), List.of("lx", "ly", "rx", "ry", "lt", "rt"));     */

        this.lectern = lectern;
    }

    private static double toDouble(boolean b) {
        return b ? 1.0 : 0.0;
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
