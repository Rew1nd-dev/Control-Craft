package com.verr1.controlcraft.unstable.pathing.dubins;

import com.verr1.controlcraft.unstable.pathing.LerpPath;
import com.verr1.controlcraft.unstable.pathing.LerpPathV2;
import org.joml.Vector2d;
import org.joml.Vector2dc;
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

    public static class PathSegment implements LerpPath<Vector2dc>, LerpPathV2<Vector2dc> {


        public final SegmentType type;
        public final double length;
        public final Vector2dc startPoint;
        public final Vector2dc endPoint;
        public final Vector2dc center;
        public final double radius;
        public final double startAngle;
        public final double endAngle;

        public PathSegment(SegmentType type, double length,
                           Vector2d startPoint, Vector2d endPoint,
                           Vector2d center, double radius,
                           double startAngle, double endAngle) {
            this.type = type;
            this.length = length;
            this.startPoint = startPoint;
            this.endPoint = endPoint;
            this.center = center;
            this.radius = radius;
            this.startAngle = startAngle;
            this.endAngle = endAngle;
        }

        @Override
        public int segments() {
            return SEGMENT_PRECISION;
        }

        @Override
        public Vector2dc lerp(int seg) {
            double t = (double) seg / SEGMENT_PRECISION;
            if (type == SegmentType.STRAIGHT) {
                return new Vector2d(
                        startPoint.x() + t * (endPoint.x() - startPoint.x()),
                        startPoint.y() + t * (endPoint.y() - startPoint.y())
                );
            } else {
                Vector2d startVec = new Vector2d(startPoint).sub(center);
                Vector2d endVec = new Vector2d(endPoint).sub(center);
                double currentRadialAngle = currentRadialAngle(startVec, endVec, t);
                return new Vector2d(
                        center.x() + radius * Math.cos(currentRadialAngle),
                        center.y() + radius * Math.sin(currentRadialAngle)
                );
            }
        }

        private double currentRadialAngle(Vector2dc startVec, Vector2dc endVec, double t) {
            double startRadialAngle = Math.atan2(startVec.y(), startVec.x());
            double endRadialAngle = Math.atan2(endVec.y(), endVec.x());
            double angularDistance;

            if (type == SegmentType.LEFT_TURN) {
                if (endRadialAngle < startRadialAngle) {
                    angularDistance = endRadialAngle - startRadialAngle + 2 * Math.PI;
                } else {
                    angularDistance = endRadialAngle - startRadialAngle;
                }
            } else {
                if (endRadialAngle < startRadialAngle) {
                    angularDistance = endRadialAngle - startRadialAngle;
                } else {
                    angularDistance = endRadialAngle - startRadialAngle - 2 * Math.PI;
                }
            }
            return startRadialAngle + t * angularDistance;
        }

        @Override
        public double length() {
            return length;
        }

        @Override
        public Vector2dc point(double length) {
            double t = length / this.length;
            if (type == SegmentType.STRAIGHT) {
                return new Vector2d(
                        startPoint.x() + t * (endPoint.x() - startPoint.x()),
                        startPoint.y() + t * (endPoint.y() - startPoint.y())
                );
            } else {
                Vector2d startVec = new Vector2d(startPoint).sub(center);
                Vector2d endVec = new Vector2d(endPoint).sub(center);
                double currentRadialAngle = currentRadialAngle(startVec, endVec, t);
                return new Vector2d(
                        center.x() + radius * Math.cos(currentRadialAngle),
                        center.y() + radius * Math.sin(currentRadialAngle)
                );
            }
        }
    }

    public static class DubinsPathResult implements LerpPath<Vector2d>, LerpPathV2<Vector2d> {

        public final PathType pathType;
        public final double totalLength;
        public final List<PathSegment> segments = new ArrayList<>();

        public DubinsPathResult(PathType pathType, double totalLength) {
            this.pathType = pathType;
            this.totalLength = totalLength;
        }

        @Override
        public int segments() {
            return segments.size() * SEGMENT_PRECISION;
        }

        @Override
        public Vector2d lerp(int seg) {
            int segmentIndex = seg / SEGMENT_PRECISION;
            int segmentOffset = seg % SEGMENT_PRECISION;

            if (segmentIndex < segments.size()) {
                PathSegment segment = segments.get(segmentIndex);
                Vector2dc point = segment.lerp(segmentOffset);
                return new Vector2d(point.x(), point.y());
            } else {
                PathSegment lastSegment = segments.get(segments.size() - 1);
                return new Vector2d(lastSegment.endPoint.x(), lastSegment.endPoint.y());
            }
        }

        @Override
        public double length() {
            return totalLength;
        }

        @Override
        public Vector2d point(double length) {
            double len0 = segments.get(0).length;
            double len1 = segments.get(1).length + len0;
            double len2 = segments.get(2).length + len1;

            if (length <= len0 && length > 0){
                return new Vector2d(segments.get(0).point(length));
            }

            if (length <= len1){
                return new Vector2d(segments.get(1).point(length - len0));
            }

            if (length <= len2){
                return new Vector2d(segments.get(2).point(length - len1));
            }

            return new Vector2d(segments.get(2).endPoint);
        }
    }

    // 主计算函数
    public static DubinsPathResult calculateDubinsPath(
            Vector2d start, double startHeading,
            Vector2d end, double endHeading,
            double radius) {

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
        double t = paths[bestIndex][1]; // 第一段长度（弧度）
        double p = paths[bestIndex][2]; // 第二段长度（直线距离或弧度）
        double q = paths[bestIndex][3]; // 第三段长度（弧度）
        PathType pathType = types[bestIndex];

        // 创建路径结果
        DubinsPathResult result = new DubinsPathResult(pathType, bestLength * radius);

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

        // 添加第一段（圆弧）
        result.segments.add(new PathSegment(
                getSegmentType(pathType, 0),
                t * radius,
                p0, p1,
                center1, radius,
                startAngle1, endAngle1
        ));

        // 添加第二段（直线或圆弧）
        if (pathType == PathType.RLR || pathType == PathType.LRL) {
            // 第二段是圆弧
            Vector2d centerMid = calculateCenter(p1, p1.angle, radius, getSegmentType(pathType, 1));
            double startAngleMid = Math.atan2(p1.y() - centerMid.y(), p1.x() - centerMid.x());
            double endAngleMid = Math.atan2(p2.y() - centerMid.y(), p2.x() - centerMid.x());

            result.segments.add(new PathSegment(
                    getSegmentType(pathType, 1),
                    p * radius,
                    p1, p2,
                    centerMid, radius,
                    startAngleMid, endAngleMid
            ));
        } else {
            // 第二段是直线
            result.segments.add(new PathSegment(
                    SegmentType.STRAIGHT,
                    p * radius,
                    p1, p2,
                    null, 0, 0, 0
            ));
        }

        // 添加第三段（圆弧）
        result.segments.add(new PathSegment(
                getSegmentType(pathType, 2),
                q * radius,
                p2, p3,
                center2, radius,
                startAngle2, endAngle2
        ));

        return result;
    }

    // 工具方法：计算圆心
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
                origin.x() + radius * Math.cos(offsetAngle), // + point.x()
                origin.y() + radius * Math.sin(offsetAngle)
        );
    }

    // 工具方法：转换到世界坐标系
    private static Vector2dWithAngle toWorldCoord(Vector2dWithAngle point, Vector2d origin, double heading, double radius) {
        double cosTheta = Math.cos(heading);
        double sinTheta = Math.sin(heading);
        return new Vector2dWithAngle(
                radius* (origin.x() + point.x() * cosTheta - point.y() * sinTheta),
                radius* (origin.y() + point.x() * sinTheta + point.y() * cosTheta),
                point.angle + heading
        );
    }

    // 工具方法：获取路径段的类型
    private static SegmentType getSegmentType(PathType pathType, int index) {
        char c = pathType.name().charAt(index);
        return c == 'L' ? SegmentType.LEFT_TURN :
                c == 'R' ? SegmentType.RIGHT_TURN :
                        SegmentType.STRAIGHT;
    }

    // Dubins 路径段计算
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

    // 角度归一化 [0, 2π)
    private static double mod2pi(double angle) {
        angle %= 2 * Math.PI;
        if (angle < 0) {
            angle += 2 * Math.PI;
        }
        return angle;
    }

    // 带角度的Vector2d扩展
    private static class Vector2dWithAngle extends Vector2d {
        public double angle;

        public Vector2dWithAngle(double x, double y, double angle) {
            super(x, y);
            this.angle = angle;
        }

        public Vector2dWithAngle() {
        }
    }

    // ================ 路径类型计算函数 ================

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
