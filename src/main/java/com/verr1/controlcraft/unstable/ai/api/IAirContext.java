package com.verr1.controlcraft.unstable.ai.api;

import net.minecraft.world.level.Level;
import net.minecraft.world.level.levelgen.Heightmap;
import org.joml.Quaterniondc;
import org.joml.Vector3d;
import org.joml.Vector3dc;

public interface IAirContext {
    Level world();

    Long shipId();

    default double height(){
        Vector3dc p = getPosition();
        return world().getHeight(Heightmap.Types.WORLD_SURFACE, (int) p.x(), (int) p.z());
    }

    default Vector3dc below(){
        return new Vector3d(getPosition()).setComponent(1, height());
    }

    double extremeRadius();

    double cruiseVelocity();

    Vector3dc getPosition();

    Vector3dc getVelocity();

    Quaterniondc getRotation();

    IAirController controller();

    Vector3dc getHeading();

    void kill();
}
