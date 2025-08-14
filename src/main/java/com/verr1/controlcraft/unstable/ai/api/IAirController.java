package com.verr1.controlcraft.unstable.ai.api;

import org.joml.Vector3dc;

public interface IAirController {

    void overrideTarget(Vector3dc target);

    void setVelocity(double v);

}
