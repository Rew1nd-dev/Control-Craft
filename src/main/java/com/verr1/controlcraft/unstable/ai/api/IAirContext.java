package com.verr1.controlcraft.unstable.ai.api;

import net.minecraft.world.level.Level;
import org.joml.Vector3dc;

public interface IAirContext {

    Level world();

    Long shipId();

    double cruiseRadius();

    double cruiseVelocity();

    double shootTolerance();

    void fireAt(Vector3dc target);

    Vector3dc getPosition();

    Vector3dc getVelocity();

    IAirController controller();

    Vector3dc getTargetPosition();

    Vector3dc getTargetVelocity();

    Vector3dc getHeading();

}
