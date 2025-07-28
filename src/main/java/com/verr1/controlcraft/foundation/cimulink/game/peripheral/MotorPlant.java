package com.verr1.controlcraft.foundation.cimulink.game.peripheral;

import com.verr1.controlcraft.content.blocks.motor.AbstractDynamicMotor;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.type.descriptive.TargetMode;
import com.verr1.controlcraft.utils.MathUtils;

import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;

public class MotorPlant extends Plant {

    private final AbstractDynamicMotor plant;



    public MotorPlant(
            AbstractDynamicMotor plant
    ) {
        super(
                new builder()
                        .in("target", t -> plant.getController().setTarget(
                                plant.getTargetMode() == TargetMode.POSITION ?
                                        MathUtils.radianReset(t):
                                        t
                                )
                        )
                        .in("lock", l -> plant.tryLock(l > 0.5))
                        .in("torque", plant::setOutputTorque)
                        .out("current", () -> plant.getController().getValue())
                        .out("angle", plant::getCachedServoAngle)
                        .out("omega", plant::getCachedServoAngularVelocity)
        );
        this.plant = plant;

    }

    private AbstractDynamicMotor plant(){
        return plant;
    }

    private static double orElse(boolean condition, double value, double defaultValue) {
        return condition ? value : defaultValue;
    }

}
