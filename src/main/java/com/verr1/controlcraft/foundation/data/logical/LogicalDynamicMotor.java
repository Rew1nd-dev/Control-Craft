package com.verr1.controlcraft.foundation.data.logical;

import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.data.control.DynamicController;
import net.minecraft.core.Direction;

import java.util.function.Consumer;

public record LogicalDynamicMotor(
        long motorShipID,
        long compShipID,
        WorldBlockPos pos,
        Direction motorDir,
        Direction compDir,
        boolean angleOrSpeed,
        boolean shouldCounter,
        boolean eliminateGravity,
        boolean free,
        double torque,
        double speedLimit,
        DynamicController controller,
        Consumer<Double> angleCallBack,
        Consumer<Double> speedCallBack
){

}
