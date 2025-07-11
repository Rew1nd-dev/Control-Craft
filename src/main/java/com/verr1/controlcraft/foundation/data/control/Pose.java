package com.verr1.controlcraft.foundation.data.control;

import org.jetbrains.annotations.NotNull;
import org.joml.Quaterniondc;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.bodies.properties.BodyPose;
import org.valkyrienskies.core.api.bodies.properties.BodyTransform;
import org.valkyrienskies.core.api.ships.properties.PhysPose;

public record Pose(Vector3dc position, Quaterniondc rotation) implements BodyPose, PhysPose{
    public static Pose of(Vector3dc pT, Quaterniondc qT) {
        return new Pose(pT, qT);
    }

    @NotNull
    @Override
    public Vector3dc getPosition() {
        return position;
    }

    @NotNull
    @Override
    public Quaterniondc getRotation() {
        return rotation;
    }

    @Override
    public Vector3dc getPos() {
        return position;
    }

    @Override
    public Quaterniondc getRot() {
        return rotation;
    }
}
