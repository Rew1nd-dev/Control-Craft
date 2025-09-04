package com.verr1.controlcraft.unstable.pathing.dubins;


import com.verr1.controlcraft.unstable.pathing.path.ArcPath;
import com.verr1.controlcraft.unstable.pathing.path.CombinedPath;
import com.verr1.controlcraft.unstable.pathing.path.IPath;
import com.verr1.controlcraft.unstable.pathing.path.LinePath;
import org.joml.*;

import java.lang.Math;
import java.util.ArrayList;
import java.util.List;

public class DubinsCalculator {

    private static final int SEGMENT_PRECISION = 30;

    public enum SegmentType {
        LEFT_TURN, STRAIGHT, RIGHT_TURN
    }

    public enum PathType {
        LSL, LSR, RSL, RSR, RLR, LRL
    }


    public static IPath calculateDubinsPath(
            Vector2d start, double startHeading,
            Vector2d end, double endHeading,
            double radius, Matrix4dc transform
    ) {

        // 计算相对位置和角度
        double dx = end.x() - start.x();
        double dy = end.y() - start.y();
        double d = Math.sqrt(dx * dx + dy * dy) / radius;

        double theta = mod2pi(Math.atan2(dy, dx));
        double alpha = mod2pi(startHeading - theta);
        double beta = mod2pi(endHeading - theta);

        // 计算所有可能的路径
        double[][] paths = new double[6][4];
        paths[0] = LSL(alpha, beta, d);
        paths[1] = LSR(alpha, beta, d);
        paths[2] = RSL(alpha, beta, d);
        paths[3] = RSR(alpha, beta, d);
        paths[4] = RLR(alpha, beta, d);
        paths[5] = LRL(alpha, beta, d);

        // 找到最短路径
        int bestIndex = -1;
        double bestLength = Double.MAX_VALUE;
        PathType[] types = {
                PathType.LSL, PathType.LSR, PathType.RSL,
                PathType.RSR, PathType.RLR, PathType.LRL
        };

        for (int i = 0; i < 6; i++) {
            if (paths[i][0] < bestLength) {
                bestLength = paths[i][0];
                bestIndex = i;
            }
        }

        if (bestIndex == -1) {
            return null; // 没有有效路径
        }

        // 提取最佳路径参数
        double t = paths[bestIndex][1];
        double p = paths[bestIndex][2];
        double q = paths[bestIndex][3];
        PathType pathType = types[bestIndex];

        // 计算各段路径（在起点坐标系下）
        Vector2dWithAngle p0 = new Vector2dWithAngle(0, 0, 0);
        Vector2dWithAngle p1 = dubinsSegment(t, p0, getSegmentType(pathType, 0));
        Vector2dWithAngle p2 = dubinsSegment(p, p1, getSegmentType(pathType, 1));
        Vector2dWithAngle p3 = dubinsSegment(q, p2, getSegmentType(pathType, 2));

        // 转换回世界坐标系
        p0 = toWorldCoord(p0, start, startHeading, radius);
        p1 = toWorldCoord(p1, start, startHeading, radius);
        p2 = toWorldCoord(p2, start, startHeading, radius);
        p3 = toWorldCoord(p3, start, startHeading, radius);

        // 计算圆心
        Vector2d center1 = calculateCenter(start, startHeading, radius, getSegmentType(pathType, 0));
        Vector2d center2 = calculateCenter(end, endHeading, radius, getSegmentType(pathType, 2));

        // 计算圆弧角度
        double startAngle1 = Math.atan2(p0.y() - center1.y(), p0.x() - center1.x());
        double endAngle1 = Math.atan2(p1.y() - center1.y(), p1.x() - center1.x());
        double startAngle2 = Math.atan2(p2.y() - center2.y(), p2.x() - center2.x());
        double endAngle2 = Math.atan2(p3.y() - center2.y(), p3.x() - center2.x());

        // 创建路径段
        List<IPath> segments = new ArrayList<>();
        // 第一段（圆弧）
        segments.add(new ArcPath(
                startAngle1,
                endAngle1,
                radius,
                transform.translate(expand(center1), new Matrix4d()),
                getSegmentType(pathType, 0) == SegmentType.RIGHT_TURN
        ));

        // 第二段（直线或圆弧）
        if (pathType == PathType.RLR || pathType == PathType.LRL) {
            Vector2d centerMid = calculateCenter(p1, p1.angle, radius, getSegmentType(pathType, 1));
            double startAngleMid = Math.atan2(p1.y() - centerMid.y(), p1.x() - centerMid.x());
            double endAngleMid = Math.atan2(p2.y() - centerMid.y(), p2.x() - centerMid.x());
            segments.add(new ArcPath(
                    startAngleMid,
                    endAngleMid,
                    radius,
                    transform.translate(expand(centerMid), new Matrix4d()),
                    getSegmentType(pathType, 1) == SegmentType.RIGHT_TURN
            ));
        } else {
            segments.add(new LinePath(
                    transform.transformPosition(new Vector3d(p1.x(), p1.y(), 0)),
                    transform.transformPosition(new Vector3d(p2.x(), p2.y(), 0))
            ));
        }

        // 第三段（圆弧）
        segments.add(new ArcPath(
                startAngle2,
                endAngle2,
                radius,
                transform.translate(expand(center2), new Matrix4d()),
                getSegmentType(pathType, 2) == SegmentType.RIGHT_TURN
        ));

        return new CombinedPath(segments);
    }

    private static Vector3dc expand(Vector2dc v){
        return new Vector3d(v.x(), v.y(), 0.0);
    }

    public static IPath dubinsMatchStart(
            Vector3dc start, Vector3dc startHeading,
            Vector3dc end, Vector3dc endHeading,
            double radius) {

        // 将不可变的Vector3dc转换为可变的Vector3d
        Vector3dc s = new Vector3d(start);
        Vector3dc sh = new Vector3d(startHeading);
        Vector3dc e = new Vector3d(end);
        Vector3dc eh = new Vector3d(endHeading);

        // 计算起点到终点的向量
        Vector3dc v2 = new Vector3d(e).sub(s);

        // 计算平面法向量：n = startHeading × (end - start)
        Vector3d n = sh.cross(v2, new Vector3d());


        // 处理共线情况
        if (n.lengthSquared() < 1e-10) {
            Vector3d aux = new Vector3d(0, 1, 0);
            n.set(sh).cross(aux);
            if (n.lengthSquared() < 1e-10) {
                aux.set(0, 0, 1);
                n.set(sh).cross(aux);
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
        Vector3d projEndHeading = new Vector3d(eh).sub(n.mul(dot, new Vector3d()));
        if (projEndHeading.lengthSquared() < 1e-10) {
            sh.cross(n, projEndHeading);
            if (projEndHeading.lengthSquared() > 1e-10) {
                projEndHeading.normalize();
            } else {
                projEndHeading.set(sh).normalize();
            }
        } else {
            projEndHeading.normalize();
        }

        // 构建平面局部坐标系
        Vector3d u = new Vector3d(sh).normalize();
        Vector3d v = n.cross(u, new Vector3d());

        v.normalize();

        // 构建变换矩阵
        Matrix4d transform = new Matrix4d().translate(s.x(), s.y(), s.z()).mul(new Matrix4d(new Matrix3d(
                u.x(), u.y(), u.z(),
                v.x(), v.y(), v.z(),
                n.x(), n.y(), n.z()
        )));

        // 计算目标点在局部坐标系的2D坐标
        double endX = v2.dot(u);
        double endY = v2.dot(v);

        // 计算目标方向在局部坐标系的2D角度
        double projX = projEndHeading.dot(u);
        double projY = projEndHeading.dot(v);
        double goalHeading = Math.atan2(projY, projX);

        // 调用2D Dubins路径计算

        return calculateDubinsPath(
                new Vector2d(0, 0), 0.0,
                new Vector2d(endX, endY), goalHeading,
                radius, transform
        );
    }


    public static IPath dubinsMatchEnd(
            Vector3dc start, Vector3dc startHeading,
            Vector3dc end, Vector3dc endHeading,
            double radius) {

        // 将不可变的Vector3dc转换为可变的Vector3d
        Vector3dc s = new Vector3d(start);
        Vector3dc e = new Vector3d(end);

        // 计算起点到终点的向量
        Vector3dc v2 = new Vector3d(e).sub(s);

        // 计算平面法向量：n = startHeading × (end - start)
        Vector3d n = new Vector3d(startHeading).cross(v2, new Vector3d());

        // 处理共线情况
        if (n.lengthSquared() < 1e-10) {
            Vector3d aux = new Vector3d(0, 1, 0);
            n.set(startHeading).cross(aux);
            if (n.lengthSquared() < 1e-10) {
                aux.set(0, 0, 1);
                n.set(startHeading).cross(aux);
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

        // 将起始方向投影到平面
        double startDot = startHeading.dot(n);
        Vector3d projStartHeading = new Vector3d(startHeading).sub(n.mul(startDot, new Vector3d()));
        if (projStartHeading.lengthSquared() < 1e-10) {
            Vector3d aux = new Vector3d(0, 1, 0);
            n.cross(aux, projStartHeading);
            if (projStartHeading.lengthSquared() > 1e-10) {
                projStartHeading.normalize();
            } else {
                projStartHeading.set(aux).normalize();
            }
        } else {
            projStartHeading.normalize();
        }

        // 将目标方向投影到平面
        double endDot = endHeading.dot(n);
        Vector3d projEndHeading = new Vector3d(endHeading).sub(n.mul(endDot, new Vector3d()));
        if (projEndHeading.lengthSquared() < 1e-10) {
            projStartHeading.cross(n, projEndHeading);
            if (projEndHeading.lengthSquared() > 1e-10) {
                projEndHeading.normalize();
            } else {
                projEndHeading.set(projStartHeading).normalize();
            }
        } else {
            projEndHeading.normalize();
        }

        // 构建平面局部坐标系
        Vector3d u = new Vector3d(projStartHeading).normalize();
        Vector3d v = n.cross(u, new Vector3d());
        v.normalize();

        // 构建变换矩阵
        Matrix4d transform = new Matrix4d().translate(s.x(), s.y(), s.z()).mul(new Matrix4d(new Matrix3d(
                u.x(), u.y(), u.z(),
                v.x(), v.y(), v.z(),
                n.x(), n.y(), n.z()
        )));

        // 计算目标点在局部坐标系的2D坐标
        double endX = v2.dot(u);
        double endY = v2.dot(v);

        // 计算目标方向在局部坐标系的2D角度
        double projX = projEndHeading.dot(u);
        double projY = projEndHeading.dot(v);
        double goalHeading = Math.atan2(projY, projX);

        // 调用2D Dubins路径计算
        return calculateDubinsPath(
                new Vector2d(0, 0), 0.0,
                new Vector2d(endX, endY), goalHeading,
                radius, transform
        );
    }

    private static Vector2d calculateCenter(Vector2d origin, double heading,
                                            double radius, SegmentType type) {
        double offsetAngle = heading;
        if (type == SegmentType.LEFT_TURN) {
            offsetAngle += Math.PI / 2;
        } else if (type == SegmentType.RIGHT_TURN) {
            offsetAngle -= Math.PI / 2;
        } else {
            return null;
        }

        return new Vector2d(
                origin.x() + radius * Math.cos(offsetAngle),
                origin.y() + radius * Math.sin(offsetAngle)
        );
    }

    private static Vector2dWithAngle toWorldCoord(Vector2dWithAngle point, Vector2d origin, double heading, double radius) {
        double cosTheta = Math.cos(heading);
        double sinTheta = Math.sin(heading);
        return new Vector2dWithAngle(
                radius * (origin.x() + point.x() * cosTheta - point.y() * sinTheta),
                radius * (origin.y() + point.x() * sinTheta + point.y() * cosTheta),
                point.angle + heading
        );
    }

    private static SegmentType getSegmentType(PathType pathType, int index) {
        char c = pathType.name().charAt(index);
        return c == 'L' ? SegmentType.LEFT_TURN :
                c == 'R' ? SegmentType.RIGHT_TURN :
                        SegmentType.STRAIGHT;
    }

    private static Vector2dWithAngle dubinsSegment(double segParam, Vector2d segInit, SegmentType segType) {
        Vector2dWithAngle segEnd = new Vector2dWithAngle();
        double x = segInit.x();
        double y = segInit.y();
        double psi = segInit instanceof Vector2dWithAngle ? ((Vector2dWithAngle) segInit).angle : 0;

        if (segType == SegmentType.LEFT_TURN) {
            segEnd.x = x + Math.sin(psi + segParam) - Math.sin(psi);
            segEnd.y = y - Math.cos(psi + segParam) + Math.cos(psi);
            segEnd.angle = psi + segParam;
        } else if (segType == SegmentType.RIGHT_TURN) {
            segEnd.x = x - Math.sin(psi - segParam) + Math.sin(psi);
            segEnd.y = y + Math.cos(psi - segParam) - Math.cos(psi);
            segEnd.angle = psi - segParam;
        } else {
            segEnd.x = x + Math.cos(psi) * segParam;
            segEnd.y = y + Math.sin(psi) * segParam;
            segEnd.angle = psi;
        }
        return segEnd;
    }

    private static double mod2pi(double angle) {
        angle %= 2 * Math.PI;
        if (angle < 0) {
            angle += 2 * Math.PI;
        }
        return angle;
    }

    private static class Vector2dWithAngle extends Vector2d {
        public double angle;

        public Vector2dWithAngle(double x, double y, double angle) {
            super(x, y);
            this.angle = angle;
        }

        public Vector2dWithAngle() {
        }
    }

    private static double[] LSL(double alpha, double beta, double d) {
        double tmp0 = d + Math.sin(alpha) - Math.sin(beta);
        double p_sq = 2 + d*d - 2*Math.cos(alpha - beta) + 2*d*(Math.sin(alpha) - Math.sin(beta));

        if (p_sq < 0) {
            return new double[]{Double.MAX_VALUE, 0, 0, 0};
        }

        double tmp1 = Math.atan2(Math.cos(beta) - Math.cos(alpha), tmp0);
        double t = mod2pi(-alpha + tmp1);
        double p = Math.sqrt(p_sq);
        double q = mod2pi(beta - tmp1);
        return new double[]{t + p + q, t, p, q};
    }

    private static double[] LSR(double alpha, double beta, double d) {
        double p_sq = -2 + d*d + 2*Math.cos(alpha - beta) + 2*d*(Math.sin(alpha) + Math.sin(beta));

        if (p_sq < 0) {
            return new double[]{Double.MAX_VALUE, 0, 0, 0};
        }

        double p = Math.sqrt(p_sq);
        double tmp2 = Math.atan2(-Math.cos(alpha) - Math.cos(beta), d + Math.sin(alpha) + Math.sin(beta)) -
                Math.atan2(-2.0, p);
        double t = mod2pi(-alpha + tmp2);
        double q = mod2pi(-beta + tmp2);
        return new double[]{t + p + q, t, p, q};
    }

    private static double[] RSL(double alpha, double beta, double d) {
        double p_sq = d*d - 2 + 2*Math.cos(alpha - beta) - 2*d*(Math.sin(alpha) + Math.sin(beta));

        if (p_sq < 0) {
            return new double[]{Double.MAX_VALUE, 0, 0, 0};
        }

        double p = Math.sqrt(p_sq);
        double tmp2 = Math.atan2(Math.cos(alpha) + Math.cos(beta), d - Math.sin(alpha) - Math.sin(beta)) -
                Math.atan2(2.0, p);
        double t = mod2pi(alpha - tmp2);
        double q = mod2pi(beta - tmp2);
        return new double[]{t + p + q, t, p, q};
    }

    private static double[] RSR(double alpha, double beta, double d) {
        double tmp0 = d - Math.sin(alpha) + Math.sin(beta);
        double p_sq = 2 + d*d - 2*Math.cos(alpha - beta) + 2*d*(Math.sin(beta) - Math.sin(alpha));

        if (p_sq < 0) {
            return new double[]{Double.MAX_VALUE, 0, 0, 0};
        }

        double tmp1 = Math.atan2(Math.cos(alpha) - Math.cos(beta), tmp0);
        double t = mod2pi(alpha - tmp1);
        double p = Math.sqrt(p_sq);
        double q = mod2pi(-beta + tmp1);
        return new double[]{t + p + q, t, p, q};
    }

    private static double[] RLR(double alpha, double beta, double d) {
        double tmp_rlr = (6 - d*d + 2*Math.cos(alpha - beta) + 2*d*(Math.sin(alpha) - Math.sin(beta))) / 8;

        if (Math.abs(tmp_rlr) > 1) {
            return new double[]{Double.MAX_VALUE, 0, 0, 0};
        }

        double p = mod2pi(2*Math.PI - Math.acos(tmp_rlr));
        double t = mod2pi(alpha - Math.atan2(Math.cos(alpha) - Math.cos(beta), d - Math.sin(alpha) + Math.sin(beta)) + p/2);
        double q = mod2pi(alpha - beta - t + p);
        return new double[]{t + p + q, t, p, q};
    }

    private static double[] LRL(double alpha, double beta, double d) {
        double tmp_lrl = (6 - d*d + 2*Math.cos(alpha - beta) + 2*d*(Math.sin(beta) - Math.sin(alpha))) / 8;

        if (Math.abs(tmp_lrl) > 1) {
            return new double[]{Double.MAX_VALUE, 0, 0, 0};
        }

        double p = mod2pi(2*Math.PI - Math.acos(tmp_lrl));
        double t = mod2pi(-alpha - Math.atan2(Math.cos(alpha) - Math.cos(beta), d + Math.sin(alpha) - Math.sin(beta)) + p/2);
        double q = mod2pi(beta - alpha - t + p);
        return new double[]{t + p + q, t, p, q};
    }
}
