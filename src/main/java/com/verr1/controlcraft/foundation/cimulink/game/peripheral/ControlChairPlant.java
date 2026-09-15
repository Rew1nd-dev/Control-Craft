package com.verr1.controlcraft.foundation.cimulink.game.peripheral;

import com.verr1.controlcraft.content.blocks.controlchair.ControlChairBlockEntity;
import com.verr1.controlcraft.foundation.input.ControlChairAxis;
import com.verr1.controlcraft.foundation.input.ControlChairButton;

public class ControlChairPlant extends Plant {

    public ControlChairPlant(ControlChairBlockEntity chair) {
        super(initContext(chair));
    }

    private static builder initContext(ControlChairBlockEntity chair) {
        builder init = new builder();

        for (ControlChairAxis axis : ControlChairAxis.VALUES) {
            final ControlChairAxis current = axis;
            init.out(current.serializedName(), () -> chair.axis(current));
        }

        for (ControlChairButton button : ControlChairButton.VALUES) {
            final ControlChairButton current = button;
            init.out(current.serializedName(), () -> chair.button(current) ? 1.0 : 0.0);
        }

        init.out("hasIn", () -> chair.acceptingInput() ? 1.0 : 0.0);
        init.out("f5", () -> chair.thirdPersonView() ? 1.0 : 0.0);
        init.out("abs_yaw", chair::absYaw);
        init.out("abs_pitch", chair::absPitch);
        init.out("abs_view_x", () -> chair.absViewForward().x());
        init.out("abs_view_y", () -> chair.absViewForward().y());
        init.out("abs_view_z", () -> chair.absViewForward().z());
        init.out("used", chair::occupiedSignal);
        return init;
    }
}
