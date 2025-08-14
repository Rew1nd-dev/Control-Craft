package com.verr1.controlcraft.unstable.ai.compact.links;

import com.verr1.controlcraft.foundation.cimulink.game.peripheral.Plant;
import com.verr1.controlcraft.unstable.blocks.cruiser.CruiserBlockEntity;

public class CruiserPlant extends Plant {

    public CruiserPlant(CruiserBlockEntity cbe) {
        super(new builder()
                .out("yaw", () -> cbe.controller().yawControl())
                .out("pitch", () -> cbe.controller().pitchControl())
                .out("roll", () -> cbe.controller().rollControl())
                .out("view_x", () -> cbe.controller().targetDirection().x())
                .out("view_y", () -> cbe.controller().targetDirection().y())
                .out("view_z", () -> cbe.controller().targetDirection().z())
                .out("frontAngle", () -> cbe.awareness().frontAngle())
        );
    }
}
