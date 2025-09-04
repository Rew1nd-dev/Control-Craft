package com.verr1.controlcraft.foundation.data.control;

import org.joml.Vector3dc;

public record ImmutableVel (Vector3dc velocity, Vector3dc angularVelocity){
    public static final ImmutableVel ZERO = new ImmutableVel(new org.joml.Vector3d(), new org.joml.Vector3d());
}
