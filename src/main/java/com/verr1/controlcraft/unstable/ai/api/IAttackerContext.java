package com.verr1.controlcraft.unstable.ai.api;

import net.minecraft.world.level.levelgen.Heightmap;
import org.joml.Vector3dc;

public interface IAttackerContext extends IAirContext{

    double cruiseRadius();

    void fireAt(Vector3dc direction);



    IAirController controller();

    double shootTolerance();

    double fireCooldown();

    Vector3dc getCruiseTarget();

    Vector3dc getGroundTarget();

    Vector3dc getGroundTargetVelocity();

    Vector3dc getAirTarget();

    Vector3dc getAirTargetVelocity();

    Vector3dc getHeading();

    boolean noGroundTarget();

    boolean hasAirThreat();

}
