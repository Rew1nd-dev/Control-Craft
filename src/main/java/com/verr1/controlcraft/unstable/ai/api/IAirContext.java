package com.verr1.controlcraft.unstable.ai.api;

import net.minecraft.world.level.Level;
import org.joml.Quaterniondc;
import org.joml.Vector3dc;

public interface IAirContext {
    Level world();

    Long shipId();

    double extremeRadius();

    double cruiseVelocity();

    Vector3dc getPosition();

    Vector3dc getVelocity();

    Quaterniondc getRotation();

    IAirController controller();

    Vector3dc getHeading();
}
