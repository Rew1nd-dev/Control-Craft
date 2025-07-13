package com.verr1.controlcraft.foundation.data.control;

import org.jetbrains.annotations.NotNull;
import org.joml.Matrix4dc;
import org.joml.Quaterniondc;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.bodies.properties.BodyKinematics;
import org.valkyrienskies.core.api.bodies.properties.BodyTransform;

import java.io.DataOutput;

public class ImmutableBodyTransform implements BodyTransform {
    @NotNull
    @Override
    public Vector3dc getPosition() {
        return null;
    }

    @NotNull
    @Override
    public Quaterniondc getRotation() {
        return null;
    }

    @NotNull
    @Override
    public Vector3dc getScaling() {
        return null;
    }

    @NotNull
    @Override
    public Vector3dc getPositionInModel() {
        return null;
    }

    @NotNull
    @Override
    public Matrix4dc getToWorld() {
        return null;
    }

    @NotNull
    @Override
    public Matrix4dc getToModel() {
        return null;
    }

    @NotNull
    @Override
    public BodyKinematics withVelocity(@NotNull Vector3dc vector3dc, @NotNull Vector3dc vector3dc1) {
        return null;
    }

    @NotNull
    @Override
    public Builder toBuilder() {
        return null;
    }

    @Override
    public void writeTransform(@NotNull DataOutput dataOutput) {

    }
}
