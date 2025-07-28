package com.verr1.controlcraft.unstable.valkyrienskies.context;

import org.joml.Vector3dc;

import java.util.List;

public record LogicalPathTarget(List<Vector3dc> wayPoints, double speed) {
}
