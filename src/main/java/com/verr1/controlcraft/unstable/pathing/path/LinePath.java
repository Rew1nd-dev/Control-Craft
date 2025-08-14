package com.verr1.controlcraft.unstable.pathing.path;

import com.verr1.controlcraft.unstable.valkyrienskies.controls.AIControlUtils;
import org.joml.Vector3d;
import org.joml.Vector3dc;

public class LinePath implements IPath{

    private final Vector3dc start;
    private final Vector3dc end;
    private final double length;

    public LinePath(Vector3dc start, Vector3dc end) {
        this.start = start;
        this.end = end;
        this.length = end.distance(start);
    }

    @Override
    public Vector3dc closestTo(Vector3dc observe) {
        double ratio = AIControlUtils.pedalFromA(start, end, observe);
        return pointInternal(ratio);
    }

    @Override
    public double closestDistanceFromStart(Vector3dc observe) {
        return closestTo(observe).distance(start());
    }

    private Vector3dc pointInternal(double ratio) {
        if (ratio < 0) {
            return start;
        }
        if (ratio > 1) {
            return end;
        }
        return start.fma(
                ratio,
                new Vector3d(end).sub(start),
                new Vector3d()
        );
    }

    @Override
    public Vector3dc point(double distance) {
        return pointInternal(distance / length);
    }

    @Override
    public double length() {
        return length;
    }

    @Override
    public Vector3dc end() {
        return end;
    }

    @Override
    public Vector3dc start() {
        return start;
    }
}
