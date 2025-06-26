package com.verr1.controlcraft.foundation.cimulink.game.peripheral;

import com.verr1.controlcraft.content.blocks.motor.AbstractDynamicMotor;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
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
                        .in("target", t -> plant.getController().setTarget(MathUtils.radianReset(t)))
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

/*
private final List<Consumer<Double>> inputHandlers = List.of(
            t -> plant().getController().setTarget(t),
            l -> plant().tryLock(l > 0.5),
            t -> plant().setOutputTorque(t)
    );

    @Override
    public List<Integer> propagateTo(int inputIndex) {
        return List.of();
    }

    @Override
    public void onInputChange(Integer... inputIndexes) {
        Arrays.stream(inputIndexes).forEach(i -> inputHandlers.get(i).accept(retrieveInput(i)));
    }

    @Override
    public void onPositiveEdge() {
        updateOutput(List.of(
                plant.getController().getValue(),
                plant.getCachedServoAngle(),
                plant.getCachedServoAngularVelocity()
        ));
    }
* */

}
