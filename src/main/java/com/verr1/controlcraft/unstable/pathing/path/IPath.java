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

    static @NotNull IPath concat(List<Vector3dc> wayPoints){
        int size = wayPoints.size();
        if(size < 2)throw new IllegalArgumentException("At least 2 points required to form a path");
        if(size == 2)return new LinePath(wayPoints.get(0), wayPoints.get(1));
        IPath[] paths = new IPath[size - 1];
        for(int i = 0; i < size - 1; i++){
            paths[i] = new LinePath(wayPoints.get(i), wayPoints.get(i + 1));
        }
        return new CombinedPath(List.of(paths));
    }

}
