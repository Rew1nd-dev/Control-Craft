package com.verr1.controlcraft.mixinducks;

import net.minecraft.core.BlockPos;

public interface ICannonDuck {

    void controlCraft$setYaw(float value);

    float controlCraft$getYaw();

    void controlCraft$setPitch(float value);

    float controlCraft$getPitch();

    void controlCraft$fire(int strength, boolean fireChanged);

    void controlCraft$assemble();

    void controlCraft$disassemble();

    BlockPos controlCraft$getBlockPos();

}
