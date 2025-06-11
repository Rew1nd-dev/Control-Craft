package com.verr1.controlcraft.foundation.cimulink.game.peripheral;

import com.verr1.controlcraft.content.blocks.motor.AbstractDynamicMotor;
import com.verr1.controlcraft.content.blocks.propeller.PropellerControllerBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;

import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;

public class PropellerPlant extends NamedComponent {


    private final PropellerControllerBlockEntity plant;

    private final List<Consumer<Double>> inputHandlers = List.of(
            t -> plant().setTargetSpeed(t)
    );

    public PropellerPlant(
            PropellerControllerBlockEntity plant
    ) {
        super(
                List.of("target"),
                List.of("")
        );
        this.plant = plant;

    }

    private PropellerControllerBlockEntity plant(){
        return plant;
    }

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
        // updateOutput(0, plant.getTargetSpeed());
    }
}
