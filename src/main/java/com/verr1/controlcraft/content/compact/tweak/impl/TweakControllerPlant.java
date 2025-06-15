package com.verr1.controlcraft.content.compact.tweak.impl;

import com.getitemfromblock.create_tweaked_controllers.block.TweakedLecternControllerBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;

import java.util.List;

public class TweakControllerPlant extends NamedComponent {

    private final TweakedLecternControllerBlockEntity lectern;

    public TweakControllerPlant(TweakedLecternControllerBlockEntity lectern) {
        super(List.of(), List.of("lx", "ly", "rx", "ry", "lt", "rt"));
        this.lectern = lectern;
    }

    @Override
    public List<Integer> propagateTo(int inputIndex) {
        return List.of();
    }

    @Override
    public void onInputChange(Integer... inputIndexes) {

    }

    @Override
    public void onPositiveEdge() {
        updateOutput(List.of(
                (double)lectern.GetAxis(0),
                (double)lectern.GetAxis(1),
                (double)lectern.GetAxis(2),
                (double)lectern.GetAxis(3),
                (double)lectern.GetAxis(4),
                (double)lectern.GetAxis(5)
        ));
    }
}
