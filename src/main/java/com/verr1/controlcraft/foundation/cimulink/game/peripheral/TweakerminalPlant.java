package com.verr1.controlcraft.foundation.cimulink.game.peripheral;

import com.verr1.controlcraft.content.links.tweakerminal.TweakerminalBlockEntity;

public class TweakerminalPlant extends Plant{

    public TweakerminalPlant(TweakerminalBlockEntity terminal) {
        super(new builder()
                .out("lx", () -> terminal.getAxis(0))
                .out("ly", () -> terminal.getAxis(1))
                .out("rx", () -> terminal.getAxis(2))
                .out("ry", () -> terminal.getAxis(3))
                .out("lt", () -> terminal.getAxis(4))
                .out("rt", () -> terminal.getAxis(5))
                .out("b0" , () -> toDouble(terminal.getButton(0 )))
                .out("b1" , () -> toDouble(terminal.getButton(1 )))
                .out("b2" , () -> toDouble(terminal.getButton(2 )))
                .out("b3" , () -> toDouble(terminal.getButton(3 )))
                .out("b4" , () -> toDouble(terminal.getButton(4 )))
                .out("b5" , () -> toDouble(terminal.getButton(5 )))
                .out("b6" , () -> toDouble(terminal.getButton(6 )))
                .out("b7" , () -> toDouble(terminal.getButton(7 )))
                .out("b8" , () -> toDouble(terminal.getButton(8 )))
                .out("b9" , () -> toDouble(terminal.getButton(9 )))
                .out("b10", () -> toDouble(terminal.getButton(10)))
                .out("b11", () -> toDouble(terminal.getButton(11)))
                .out("b12", () -> toDouble(terminal.getButton(12)))
                .out("b13", () -> toDouble(terminal.getButton(13)))
                .out("b14", () -> toDouble(terminal.getButton(14)))
                
        );
    }

    public static double toDouble(boolean b){
        return b ? 1.0 : 0.0;
    }



}
