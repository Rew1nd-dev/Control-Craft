package com.verr1.controlcraft.unstable.pathing.path;

import org.jetbrains.annotations.NotNull;
import org.joml.Vector3dc;

import java.util.List;

public interface IPath {

    Vector3dc closestTo(Vector3dc observe);

    double closestDistanceFromStart(Vector3dc observe);

    Vector3dc point(double distance);

    double length();

    Vector3dc end();

    Vector3dc start();

    static @NotNull IPath concat(@NotNull IPath... ps){
        return new CombinedPath(List.of(ps));
    }

}
