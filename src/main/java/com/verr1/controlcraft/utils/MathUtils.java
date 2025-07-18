package com.verr1.controlcraft.utils;

import net.minecraft.util.Mth;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.joml.primitives.AABBd;
import org.joml.primitives.AABBdc;

import java.util.ArrayList;
import java.util.List;

public class MathUtils {

    public static final double EXP_1 = Math.exp(1);

    public static final double eps = 1e-10;

    public static double clamp(double value, double min, double max) {
        return Math.max(min, Math.min(max, value));
    }

    public static double clamp(double value, double max) {
        max = Math.abs(max);
        return clamp(value, -max, max);
    }

    public static double safeDiv(double x, double y){
        if(Math.abs(y) < eps && Math.abs(x) < eps){
            return 0;
        }
        if(Math.abs(y) < eps){
            return x > 0 ? 1e20 : -1e20;
        }
        return x / y;
    }

    public static double safeAsin(double x){
        if(Math.abs(x) > 1){
            return x > 0 ? Math.PI / 2 : -Math.PI / 2;
        }
        return Math.asin(x);
    }

    public static double safeAcos(double x){
        if(Math.abs(x) > 1){
            return x > 0 ? 0 : Math.PI;
        }
        return Math.acos(x);
    }




    public static double clamp1(double x){
        return Math.atan(x) / Math.PI * 0.5;
    }


    public static Vector3d abs(Vector3dc v){
        return new Vector3d(Math.abs(v.x()), Math.abs(v.y()), Math.abs(v.z()));
    }

    public static Vector3d clamp(Vector3dc value, double max) {
        double x = clamp(value.x(), max);
        double y = clamp(value.y(), max);
        double z = clamp(value.z(), max);
        return new Vector3d(x, y, z);
    }

    public static double clampHalf(double x, double max){
        return Math.min(max, Math.max(0, x));
    }

    public static double relu(double x){
        return Math.max(0, x);
    }

    public static int max(Integer... ints){
        int max = Integer.MIN_VALUE;
        for(int i : ints){
            max = Math.max(max, i);
        }
        return max;
    }

    public static double radErrFix(double err){
        if(err > Math.PI){
            return err - 2 * Math.PI;
        }
        if(err < -Math.PI){
            return err + 2 * Math.PI;
        }
        return err;
    }

    public static float angleReset(float angle){
        while(angle > 180){
            angle -= 360;
        }
        while(angle < -180){
            angle += 360;
        }
        return angle;
    }

    // a bit cursed
    public static double angleReset(double angle){
        return Math.IEEEremainder(angle, 360);
    }

    // a bit cursed
    public static double radianReset(double radian){
        return Math.IEEEremainder(radian, 2 * Math.PI);
    }

    public static double toControlCraftAngular(double createSpeed){
        return createSpeed / 60 * 2 * Math.PI;
    }

    public static double toCreateAngular(double controlcraftSpeed){
        return controlcraftSpeed * 60 / (2 * Math.PI);
    }

    public static double toControlCraftLinear(double createSpeed){
        return Mth.clamp(((float) createSpeed) / 512, -.49f, .49f) * 20;
    }

    public static double toCreateLinear(double controlcraftSpeed){
        return 512 * ((float) Mth.clamp(controlcraftSpeed / 20f, -.49f, .49f));
    }

    public static AABBd coverOf(List<Vector3dc> points){
        double minX = Double.POSITIVE_INFINITY;
        double minY = Double.POSITIVE_INFINITY;
        double minZ = Double.POSITIVE_INFINITY;
        double maxX = Double.NEGATIVE_INFINITY;
        double maxY = Double.NEGATIVE_INFINITY;
        double maxZ = Double.NEGATIVE_INFINITY;
        for(Vector3dc point : points){
            minX = Math.min(minX, point.x());
            minY = Math.min(minY, point.y());
            minZ = Math.min(minZ, point.z());
            maxX = Math.max(maxX, point.x());
            maxY = Math.max(maxY, point.y());
            maxZ = Math.max(maxZ, point.z());
        }
        return new AABBd(minX, minY, minZ, maxX, maxY, maxZ);
    }

    public static double clampDigit(double value, int digits){
        return Math.round(value * Math.pow(10, digits)) / Math.pow(10, digits);
    }

    public static AABBd centerWithRadius(Vector3dc center, double r){
        return new AABBd(center.x() - r, center.y() - r, center.z() - r, center.x() + r, center.y() + r, center.z() + r);
    }

    public static ArrayList<Vector3dc> pointOf(AABBdc aabBdc){
        return new ArrayList<>(List.of(
                new Vector3d(aabBdc.minX(), aabBdc.minY(), aabBdc.minZ()),
                new Vector3d(aabBdc.maxX(), aabBdc.minY(), aabBdc.minZ()),
                new Vector3d(aabBdc.minX(), aabBdc.maxY(), aabBdc.minZ()),
                new Vector3d(aabBdc.maxX(), aabBdc.maxY(), aabBdc.minZ()),
                new Vector3d(aabBdc.minX(), aabBdc.minY(), aabBdc.maxZ()),
                new Vector3d(aabBdc.maxX(), aabBdc.minY(), aabBdc.maxZ()),
                new Vector3d(aabBdc.minX(), aabBdc.maxY(), aabBdc.maxZ()),
                new Vector3d(aabBdc.maxX(), aabBdc.maxY(), aabBdc.maxZ())
        ));
    }

    public static Vector3d safeNormalize(Vector3dc hvt) {
        double length = hvt.length();
        if (length == 0) {
            return new Vector3d(0, 0, 0);
        }
        return new Vector3d(hvt.x() / length, hvt.y() / length, hvt.z() / length);
    }
}
