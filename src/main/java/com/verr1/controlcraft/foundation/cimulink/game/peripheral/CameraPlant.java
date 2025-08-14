package com.verr1.controlcraft.foundation.cimulink.game.peripheral;

import com.verr1.controlcraft.content.blocks.camera.CameraBlockEntity;

public class CameraPlant extends Plant{

    public CameraPlant(CameraBlockEntity cbe) {
        super(new builder()
                .out("yaw", cbe::getTransformedYaw)
                .out("pitch", cbe::getPitch)
                .out("abs_yaw", cbe::getYaw)
                .out("abs_pitch", cbe::getTransformedPitch)
                .out("abs_view_x", () -> cbe.getAbsViewForward().x())
                .out("abs_view_y", () -> cbe.getAbsViewForward().y())
                .out("abs_view_z", () -> cbe.getAbsViewForward().z())
        );
    }
}
