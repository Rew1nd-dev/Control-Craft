package com.verr1.controlcraft.unstable.ai.api;

import org.joml.Vector3dc;

public interface IAirCannon {

    int getCooldown();

    void fireAt(Vector3dc direction);

}
