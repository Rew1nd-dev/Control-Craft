package com.verr1.controlcraft.unstable.ai.api;

import net.minecraft.world.level.levelgen.Heightmap;
import org.joml.Vector3dc;

public interface IAttackerContext extends IAirContext{

    double extremeRadius();

    void fireAt(Vector3dc direction);

    default double height(){
        Vector3dc p = getPosition();
        return world().getHeight(Heightmap.Types.MOTION_BLOCKING, (int) p.x(), (int) p.z());
    }

    IAirController controller();

    Vector3dc getGroundTarget();

    Vector3dc getGroundTargetVelocity();

    Vector3dc getAirTarget();

    Vector3dc getAirTargetVelocity();

    Vector3dc getHeading();

    boolean noGroundTarget();

    boolean hasAirThreat();

}
