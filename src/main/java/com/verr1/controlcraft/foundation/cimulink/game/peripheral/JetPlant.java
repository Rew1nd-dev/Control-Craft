package com.verr1.controlcraft.foundation.cimulink.game.peripheral;

import com.verr1.controlcraft.content.blocks.jet.JetBlockEntity;
import com.verr1.controlcraft.content.blocks.propeller.PropellerControllerBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;

import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;

public class JetPlant extends Plant {

    private final JetBlockEntity plant;



    public JetPlant(
            JetBlockEntity plant
    ) {
        super(new builder()
                .in("thrust", t -> plant.thrust.write(t))
                .in("horizontal",  t -> plant.horizontalAngle.write(t))
                .in("vertical", t -> plant.verticalAngle.write(t))
        );

        this.plant = plant;

    }

    private JetBlockEntity plant(){
        return plant;
    }

    /*
    private final List<Consumer<Double>> inputHandlers = List.of(
            t -> plant().thrust.write(t),
            t -> plant().horizontalAngle.write(t),
            t -> plant().verticalAngle.write(t)
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
        // updateOutput(0, plant.getTargetSpeed());
    }
    * */

}
