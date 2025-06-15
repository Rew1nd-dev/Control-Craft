package com.verr1.controlcraft.foundation.cimulink.game.peripheral;

import com.verr1.controlcraft.content.blocks.flap.CompactFlapBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;

import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;

public class FlapPlant extends NamedComponent {

    private final CompactFlapBlockEntity cfb;

    private final List<Consumer<Double>> inputHandlers = List.of(
            t -> plant().setAttackAngle(t)
    );

    public FlapPlant(CompactFlapBlockEntity cfb) {
        super(
                List.of("angle"),
                List.of()
        );
        this.cfb = cfb;
    }

    @Override
    public List<Integer> propagateTo(int inputIndex) {
        return List.of();
    }

    private CompactFlapBlockEntity plant() {
        return cfb;
    }

    @Override
    public void onInputChange(Integer... inputIndexes) {
        Arrays.stream(inputIndexes).forEach(i -> inputHandlers.get(i).accept(retrieveInput(i)));
    }

    @Override
    public void onPositiveEdge() {

    }
}
