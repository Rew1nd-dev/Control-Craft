package com.verr1.controlcraft.foundation.cimulink.game.peripheral;

import com.verr1.controlcraft.content.blocks.camera.CameraBlockEntity;

public class CameraPlant extends Plant{

    public CameraPlant(CameraBlockEntity cbe) {
        super(new builder()
                .out("yaw", cbe::getYaw)
                .out("pitch", cbe::getPitch)
        );
    }
}
