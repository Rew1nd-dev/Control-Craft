package com.verr1.controlcraft.foundation.cimulink.game.peripheral;

import com.verr1.controlcraft.content.blocks.motor.AbstractDynamicMotor;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;

import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;

public class MotorPlant extends NamedComponent {

    private final AbstractDynamicMotor plant;

    private final List<Consumer<Double>> inputHandlers = List.of(
            t -> plant().getController().setTarget(t),
            l -> plant().tryLock(l > 0.5),
            t -> plant().setOutputTorque(t)
    );

    public MotorPlant(
            AbstractDynamicMotor plant
    ) {
        super(
                List.of("target", "lock", "torque"),
                List.of("current")
        );
        this.plant = plant;

    }

    private AbstractDynamicMotor plant(){
        return plant;
    }

    @Override
    public List<Integer> propagateTo(int inputIndex) {
        return List.of(0);
    }

    @Override
    public void onInputChange(Integer... inputIndexes) {
        Arrays.stream(inputIndexes).forEach(i -> inputHandlers.get(i).accept(retrieveInput(i)));
    }

    @Override
    public void onPositiveEdge() {
        updateOutput(0, plant.getController().getValue());
    }
}
