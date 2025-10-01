package com.verr1.controlcraft.content.compact.vssw.impl;

import com.verr1.controlcraft.foundation.cimulink.game.peripheral.MutablePlant;
import com.verr1.controlcraft.foundation.cimulink.game.peripheral.Plant;
import net.shao.valkyrien_space_war.block.seat.base.BaseShipControlSeatBE;

import java.util.Optional;

public class ControlSeatPlant extends Plant {



    public ControlSeatPlant(BaseShipControlSeatBE seat) {
        super(new builder()
                .out("FrontBack", () -> safeGet(seat.input, 0) - safeGet(seat.input, 1))
                .out("LeftRight", () -> safeGet(seat.input, 3) - safeGet(seat.input, 2))
                .out("UpDown", () -> safeGet(seat.input, 4) - safeGet(seat.input, 5))
                .out("Roll", () -> safeGet(seat.input, 7) - safeGet(seat.input, 6))
                .out("Yaw", () -> safeGet(seat.input, 9) - safeGet(seat.input, 8))
                .out("Pitch", () -> safeGet(seat.input, 10) - safeGet(seat.input, 11))
                .out("Fire", () -> safeGet(seat.input, 12))
        );
    }


    public static double safeGet(float[] input, int index){
        if(input == null)return 0.0;
        if(index < 0 || index >= input.length)return 0.0;
        return input[index];
    }

}
