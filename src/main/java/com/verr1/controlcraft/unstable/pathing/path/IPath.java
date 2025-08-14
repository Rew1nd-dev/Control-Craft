package com.verr1.controlcraft.unstable.pathing.path;

import org.joml.Vector3dc;

public interface IPath {

    Vector3dc closestTo(Vector3dc observe);

    double closestDistanceFromStart(Vector3dc observe);

    Vector3dc point(double distance);

    double length();

    Vector3dc end();

    Vector3dc start();



}
