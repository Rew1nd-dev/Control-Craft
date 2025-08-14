package com.verr1.controlcraft.unstable.pathing;

import com.verr1.controlcraft.unstable.pathing.dubins.DubinsCalculator;
import org.joml.Vector2d;
import org.joml.Vector3d;
import org.joml.Vector3dc;

public class Navigator {


    public static LerpPathV2<Vector3dc> dubins(
            Vector3dc start, Vector3dc startHeading,
            Vector3dc end, Vector3dc endHeading,
            double radius
    ) {

        // 将不可变的Vector3dc转换为可变的Vector3d
        Vector3d s = new Vector3d(start);
        Vector3d sh = new Vector3d(startHeading);
        Vector3d e = new Vector3d(end);
        Vector3d eh = new Vector3d(endHeading);

        // 计算起点到终点的向量
        Vector3d v2 = new Vector3d(e).sub(s);

        // 计算平面法向量：n = startHeading × (end - start)
        Vector3d n = new Vector3d();
        sh.cross(v2, n);

        // 处理共线情况（法向量长度接近0）
        if (n.lengthSquared() < 1e-10) {
            // 尝试使用y轴作为辅助向量
            Vector3d aux = new Vector3d(0, 1, 0);
            n.set(sh).cross(aux);
            if (n.lengthSquared() < 1e-10) {
                // 再尝试z轴
                aux.set(0, 0, 1);
                n.set(sh).cross(aux);
                // 如果仍然共线，使用默认法向量 (0,0,1)
                if (n.lengthSquared() < 1e-10) {
                    n.set(0, 0, 1);
                } else {
                    n.normalize();
                }
            } else {
                n.normalize();
            }
        } else {
            n.normalize();
        }

        // 将目标方向投影到平面
        double dot = eh.dot(n);
        Vector3d projEndHeading = new Vector3d(eh).sub(n.mul(dot));

        // 处理投影后零向量
        if (projEndHeading.lengthSquared() < 1e-10) {
            // 如果投影为零，使用平面内与起始方向垂直的向量
            Vector3d temp = new Vector3d();
            sh.cross(n, temp);
            if (temp.lengthSquared() > 1e-10) {
                projEndHeading.set(temp).normalize();
            } else {
                // 最后保底：使用起始方向
                projEndHeading.set(sh).normalize();
            }
        } else {
            projEndHeading.normalize();
        }

        // 构建平面局部坐标系
        Vector3d u = new Vector3d(sh).normalize(); // 局部坐标系x轴（起始方向）
        Vector3d v = new Vector3d();              // 局部坐标系y轴
        n.cross(u, v);
        v.normalize();

        // 计算目标点在局部坐标系的2D坐标
        double endX = v2.dot(u);
        double endY = v2.dot(v);

        // 计算目标方向在局部坐标系的2D角度
        double projX = projEndHeading.dot(u);
        double projY = projEndHeading.dot(v);
        double goalHeading = Math.atan2(projY, projX);

        // 调用2D Dubins路径计算
        LerpPathV2<Vector2d> path2D = calculateDubinsPathV2(
                new Vector2d(0, 0), 0.0,
                new Vector2d(endX, endY), goalHeading,
                radius
        );
        if(path2D == null)return null;
        // 将2D路径转换为3D路径
        return new LerpPathV2<>() {
            @Override
            public double length() {
                return path2D.length();
            }

            @Override
            public Vector3dc point(double length) {
                Vector2d point2D = path2D.point(length);
                Vector3d point3D = new Vector3d(s);
                point3D.add(u.mul(point2D.x, new Vector3d()));
                point3D.add(v.mul(point2D.y, new Vector3d()));
                return point3D;
            }


        };
    }


    public static LerpPath<Vector2d> calculateDubinsPath(
            Vector2d startPos, double startHeading,
            Vector2d goalPos, double goalHeading,
            double turningRadius
    ){
        return DubinsCalculator.calculateDubinsPath(
                startPos, startHeading,
                goalPos, goalHeading,
                turningRadius
        );
    }

    public static LerpPathV2<Vector2d> calculateDubinsPathV2(
            Vector2d startPos, double startHeading,
            Vector2d goalPos, double goalHeading,
            double turningRadius
    ){
        return DubinsCalculator.calculateDubinsPath(
                startPos, startHeading,
                goalPos, goalHeading,
                turningRadius
        );
    }

}
