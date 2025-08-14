package com.verr1.controlcraft.unstable.pathing.path;

import com.verr1.controlcraft.utils.MathUtils;
import org.joml.*;

import java.lang.Math;

public class ArcPath implements IPath{

    private final double startAngle2D;
    private final double endAngle2D;
    private final double radius;
    private final Matrix4dc transform;
    private final boolean clockwise;

    private final double angleSpan;

    public ArcPath(
            double startAngle2D,
            double endAngle2D,
            double radius,
            Matrix4dc transform,
            boolean clockwise
    ) {
        this.startAngle2D = startAngle2D;
        this.endAngle2D = endAngle2D;
        this.radius = radius;
        this.transform = transform;
        this.clockwise = clockwise;
        this.angleSpan = span(startAngle2D, endAngle2D, clockwise);
    }

    private static double span(double start, double end, boolean clockwise){
        double min = Math.min(start, end);
        double max = Math.max(start, end);
        double delta = max - min;
        if(!rightArc(start, end, clockwise)){
            delta = 2 * Math.PI - delta;
        }

        return delta;
    }


    @Override
    public Vector3dc closestTo(Vector3dc observe) {
        return point(closestDistanceFromStart(observe));
    }

    @Override
    public double closestDistanceFromStart(Vector3dc observe) {
        Vector3dc local = toLocal(observe);
        double angle = Math.atan2(local.y(), local.x());
        if(inRange(angle)){
            double rev = reverseLerpInternal(angle);
            return rev * length();
        }
        if(angleDiff(startAngle2D, angle) < angleDiff(endAngle2D, angle)){
            return 0;
        } else {
            return length();
        }
    }

    private static double angleDiff(double a0, double a1){
        double delta = Math.max(a0, a1) - Math.min(a0, a1);
        double delta1 = 2 * Math.PI - delta;
        return Math.min(delta1, delta);
    }

    private boolean rightArc(){
        return rightArc(startAngle2D, endAngle2D, clockwise);
    }

    private static boolean rightArc(double start, double end, boolean clockwise){
        return clockwise && start > end ||
                !clockwise && start < end;
    }

    private boolean inRange(double angle){
        double min = Math.min(startAngle2D, endAngle2D);
        double max = Math.max(startAngle2D, endAngle2D);
        if(rightArc()){
            return angle >= min && angle <= max;
        }
        return angle <= min || angle >= max;
    }

    @Override
    public Vector3dc point(double distance) {
        return toWorld(
                pointInternal(
                        MathUtils.clamp(distance / length(), 0, 1)
                )
        );
    }

    private Vector2d pointInternal(double ratio) {
        double angle = lerpInternal(ratio);
        return new Vector2d(
                radius * Math.cos(angle),
                radius * Math.sin(angle)
        );
    }

    private double lerpInternal(double ratio) {
        double sign = clockwise ? -1 : 1;
        double angle = sign * ratio * angleSpan + startAngle2D;
        return MathUtils.radianReset(angle);
    }

    private double reverseLerpInternal(double angle) {
        double span = span(startAngle2D, angle, clockwise);
        return span / angleSpan;
    }

    public Vector3dc toWorld(Vector2dc plane){
        return transform.transformPosition(
                new Vector3d(plane.x(), plane.y(), 0.0)
        );
    }

    public Vector3dc toLocal(Vector3dc point){
        return transform.invert(new Matrix4d())
                .transformPosition(new Vector3d(point));
    }

    @Override
    public double length() {
        return angleSpan * radius;
    }

    @Override
    public Vector3dc end() {
        return toWorld(
                pointInternal(1)
        );
    }

    @Override
    public Vector3dc start() {
        return toWorld(
                pointInternal(0)
        );
    }
}
