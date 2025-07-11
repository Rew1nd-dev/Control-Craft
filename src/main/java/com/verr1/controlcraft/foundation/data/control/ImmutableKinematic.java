package com.verr1.controlcraft.foundation.data.control;

import org.jetbrains.annotations.NotNull;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.bodies.properties.BodyKinematics;
import org.valkyrienskies.core.api.bodies.properties.BodyTransform;

public record ImmutableKinematic() implements BodyKinematics {
    @NotNull
    @Override
    public BodyTransform getTransform() {
        return null;
    }

    @NotNull
    @Override
    public Vector3dc getVelocity() {

        return null;
    }

    @NotNull
    @Override
    public Vector3dc getAngularVelocity() {
        return null;
    }

    @NotNull
    @Override
    public BodyKinematics withTransform(@NotNull BodyTransform bodyTransform) {
        return null;
    }

    @NotNull
    @Override
    public Builder toBuilder() {
        return null;
    }
}
