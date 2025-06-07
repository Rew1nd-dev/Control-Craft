package com.verr1.controlcraft.utils;

import net.minecraft.world.phys.Vec3;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.ArrayList;
import java.util.List;

public class BezierCurve {
    public static List<Vector3d> calculateCubicBezier(Vector3dc p0, Vector3dc p1, Vector3dc p2, Vector3dc p3, int segments) {
        List<Vector3d> points = new ArrayList<>();
        for (int i = 0; i <= segments; i++) {
            float t = i / (float) segments;
            points.add(calculatePoint(p0, p1, p2, p3, t));
        }
        return points;
    }

    private static Vector3d calculatePoint(Vector3dc p0, Vector3dc p1, Vector3dc p2, Vector3dc p3, float t) {
        double u = 1 - t;
        double tt = t * t;
        double uu = u * u;
        double uuu = uu * u;
        double ttt = tt * t;

        double x = uuu * p0.x() + 3 * uu * t * p1.x() + 3 * u * tt * p2.x() + ttt * p3.x();
        double y = uuu * p0.y() + 3 * uu * t * p1.y() + 3 * u * tt * p2.y() + ttt * p3.y();
        double z = uuu * p0.z() + 3 * uu * t * p1.z() + 3 * u * tt * p2.z() + ttt * p3.z();

        return new Vector3d(x, y, z);
    }
}
