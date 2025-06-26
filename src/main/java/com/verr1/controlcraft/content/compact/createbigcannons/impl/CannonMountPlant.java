package com.verr1.controlcraft.content.compact.createbigcannons.impl;

import com.verr1.controlcraft.foundation.cimulink.game.peripheral.Plant;
import com.verr1.controlcraft.mixinducks.ICannonDuck;
import org.jetbrains.annotations.NotNull;

public class CannonMountPlant extends Plant {

    protected CannonMountPlant(@NotNull ICannonDuck cannon) {
        super(new builder()
                .out("pitch", () -> (double)cannon.controlCraft$getPitch())
                .out("yaw", () -> (double)cannon.controlCraft$getYaw())
        );
    }
}
