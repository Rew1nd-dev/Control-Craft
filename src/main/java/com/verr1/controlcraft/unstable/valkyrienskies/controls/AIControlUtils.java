package com.verr1.controlcraft.unstable.valkyrienskies.controls;

import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.List;

public class AIControlUtils {


    public static double circumcircleRadius(Vector3dc A, Vector3dc B, Vector3dc C) {
        // 1. 计算各边长度
        double a = B.distance(C); // BC边长
        double b = A.distance(C); // AC边长
        double c = A.distance(B); // AB边长

        // 2. 计算三角形面积（使用向量叉积）
        Vector3d AB = new Vector3d(B).sub(A);
        Vector3d AC = new Vector3d(C).sub(A);
        Vector3d cross = new Vector3d();
        AB.cross(AC, cross);
        double area = 0.5 * cross.length(); // K = 1/2 * |AB × AC|

        // 3. 处理退化情况（三点共线）
        if (area < 1e-10) {
            return Double.POSITIVE_INFINITY; // 返回无穷大表示直线
        }

        // 4. 计算外接圆半径
        return (a * b * c) / (4 * area);
    }

    public static int closestIndex(List<Vector3dc> curve, Vector3dc position){
        int result = 0;
        double minDistance = Double.POSITIVE_INFINITY;
        for (int i = 0; i < curve.size(); i++) {
            Vector3dc point = curve.get(i);
            double distance = point.distance(position);
            if (distance < minDistance) {
                minDistance = distance;
                result = i;
            }
        }
        return result;
    }

    public static Vector3dc projection(Vector3dc v, Vector3dc ref){
        if(ref.lengthSquared() < 1e-8)return v;
        return new Vector3d(ref).mul(v.dot(ref) / ref.lengthSquared());
    }

    public static boolean sameSide(Vector3dc v0, Vector3dc v1, Vector3dc ref){
        Vector3dc tang0 = v0.sub(projection(v0, ref), new Vector3d());
        Vector3dc tang1 = v1.sub(projection(v1, ref), new Vector3d());

        return tang1.dot(tang0) > 0;

    }

    public static Vector3dc projection(Vector3dc v, Vector3dc refX, Vector3dc refY){
// 计算法向量 n = refX × refY
        Vector3d n = new Vector3d(refX).cross(refY);

        // 计算 n 的点积 (n · n)
        double nDotN = n.dot(n);

        // 检查 refX 和 refY 是否平行（法向量为零）
        if (Math.abs(nDotN) < 1e-10) { // 使用小阈值避免浮点误差
            throw new IllegalArgumentException("Vectors refX and refY are parallel; cannot define refX plane.");
        }

        // 计算 v · n
        double cDotN = v.dot(n);

        // 计算标量因子 k = (v · n) / (n · n)
        double k = cDotN / nDotN;

        // 计算法向投影： proj_n = k * n
        Vector3d projN = new Vector3d(n).mul(k);

        // 计算平面投影： proj_plane = v - proj_n

        return new Vector3d(v).sub(projN);
    }

    public static Vector3dc aimPredict(Vector3dc p_t, Vector3dc v_t, Vector3dc p_c, double v_b){

        Vector3d p_rel = new Vector3d(p_t).sub(p_c);

        // 计算目标速度的平方模长
        double v_t_mag_sq = v_t.lengthSquared();

        // 计算二次方程的系数
        double a = v_t_mag_sq - (v_b * v_b);
        double b = 2.0 * v_t.dot(p_rel);
        double c = p_rel.lengthSquared();

        // 计算判别式
        double delta = b * b - 4 * a * c;

        // 如果判别式小于0，无实数解
        if (delta < 0) {
            return null;
        }

        // 计算两个可能的时间解
        double sqrtDelta = Math.sqrt(delta);
        double t1 = (-b + sqrtDelta) / (2 * a);
        double t2 = (-b - sqrtDelta) / (2 * a);

        // 寻找最小的正时间解
        double t_hit = Double.POSITIVE_INFINITY;
        if (t1 > 0) t_hit = t1;
        if (t2 > 0 && t2 < t_hit) t_hit = t2;

        // 如果没有有效解
        if (t_hit == Double.POSITIVE_INFINITY) {
            return null;
        }

        // 计算目标在命中时刻的位置: p_t + v_t * t_hit
        return new Vector3d(v_t).mul(t_hit).add(p_t);
    }

    public static Vector3d tangent(Vector3dc v, Vector3dc ref){
        return v.sub(pedal(ref, new Vector3d(), v), new Vector3d());
    }

    public static Vector3d pedal(Vector3dc A, Vector3dc B, Vector3dc P) {
        Vector3d AB = new Vector3d(B).sub(A); // Vector AB
        Vector3d AP = new Vector3d(P).sub(A); // Vector AP

        double abLengthSquared = AB.lengthSquared();
        if (abLengthSquared < 1e-10) {
            // Handle degenerate case where A and B are the same point
            return new Vector3d(A);
        }

        // Project AP onto AB, clamping the result to the range [0, 1]
        double t = AP.dot(AB) / abLengthSquared;
        t = Math.max(0, Math.min(1, t));

        // Compute the closest point on the line segment
        return new Vector3d(A).fma(t, AB);

    }

    public static Vector3d circumcenter(Vector3dc a, Vector3dc b, Vector3dc c) {
        Vector3d abMid = new Vector3d(a).add(b).mul(0.5); // Midpoint of AB
        Vector3d bcMid = new Vector3d(b).add(c).mul(0.5); // Midpoint of BC

        Vector3d abDir = new Vector3d(b).sub(a); // Direction of AB
        Vector3d bcDir = new Vector3d(c).sub(b); // Direction of BC

        // Perpendicular vectors to AB and BC
        Vector3d abPerp = new Vector3d(-abDir.y(), abDir.x(), 0);
        Vector3d bcPerp = new Vector3d(-bcDir.y(), bcDir.x(), 0);

        // Solve for the intersection of the two lines
        double denominator = abPerp.x() * bcPerp.y() - abPerp.y() * bcPerp.x();
        if (Math.abs(denominator) < 1e-10) {
            return new Vector3d(abMid); // Lines are parallel, return midpoint of AB
        }

        double t = ((bcMid.x() - abMid.x()) * bcPerp.y() - (bcMid.y() - abMid.y()) * bcPerp.x()) / denominator;

        // Circumcenter is the intersection point
        return new Vector3d(abMid).fma(t, abPerp);
    }

}
