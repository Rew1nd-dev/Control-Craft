package com.verr1.controlcraft.foundation.cimulink.game.peripheral;

import com.verr1.controlcraft.content.blocks.flap.CompactFlapBlockEntity;
import com.verr1.controlcraft.content.blocks.slider.DynamicSliderBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;

import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;

public class SliderPlant extends NamedComponent {
    private final DynamicSliderBlockEntity dsb;

    private final List<Consumer<Double>> inputHandlers = List.of(
            t -> plant().setTarget(t),
            l -> plant().tryLock(l > 0.5),
            f -> plant().setOutputForce(f)
    );

    public SliderPlant(DynamicSliderBlockEntity dsb) {
        super(
                List.of("target", "lock", "torque"),
                List.of("current", "distance")
        );
        this.dsb = dsb;
    }

    @Override
    public List<Integer> propagateTo(int inputIndex) {
        return List.of(0, 1, 2);
    }

    private DynamicSliderBlockEntity plant() {
        return dsb;
    }

    @Override
    public void onInputChange(Integer... inputIndexes) {
        Arrays.stream(inputIndexes).forEach(i -> inputHandlers.get(i).accept(retrieveInput(i)));
    }

    @Override
    public void onPositiveEdge() {
        updateOutput(List.of(
                plant().getController().getValue(),
                plant().getSlideDistance()
        ));
    }

}
