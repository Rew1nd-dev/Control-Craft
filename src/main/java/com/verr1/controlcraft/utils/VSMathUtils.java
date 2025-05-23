package com.verr1.controlcraft.utils;



import com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntity;
import org.joml.*;
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.core.api.ships.Ship;


import javax.annotation.Nullable;
import java.lang.Math;
import java.util.ArrayList;
import java.util.List;

//XC2YC:  [X, Y, Z]: X: the X unit basis in YC coordinate represented by XC coordinate unit vector basis,
//                      such that XC2YC * v_x(represented in XC coordinate) = v_y(represented in YC coordinate)
public class VSMathUtils {

    // quaternion to rotate UP to facing
    public static Quaterniond getQuaternionOfPlacement(Direction facing){
        return switch (facing){
            case DOWN -> new Quaterniond(new AxisAngle4d(Math.PI, new Vector3d(1.0, 0.0, 0.0)));
            case NORTH -> (new Quaterniond(new AxisAngle4d(Math.PI, new Vector3d(0.0, 1.0, 0.0)))).mul((new Quaterniond(new AxisAngle4d(Math.PI / 2, (new Vector3d(1.0, 0.0, 0.0)))))).normalize();
            case EAST -> new Quaterniond(new AxisAngle4d(Math.PI / 2,  new Vector3d(0.0, 1.0, 0.0))).mul(new Quaterniond(new AxisAngle4d(Math.PI / 2, (new Vector3d(1.0, 0.0, 0.0))))).normalize();
            case SOUTH -> (new Quaterniond(new AxisAngle4d(Math.PI / 2, (new Vector3d(1.0, 0.0, 0.0))))).normalize();
            case WEST ->(new Quaterniond(new AxisAngle4d(Math.PI * 3 / 2, (new Vector3d(0.0, 1.0, 0.0))))).mul((Quaterniondc)(new Quaterniond(new AxisAngle4d(Math.PI / 2, (new Vector3d(1.0, 0.0, 0.0)))))).normalize();
            default -> new Quaterniond();
        };
    }
    // EAST: +x
    // SOUTH: +z
    // UP: +y
    public static Quaterniond getQuaternionToEast(Direction facing){
        return switch (facing){
            case DOWN -> new Quaterniond(new AxisAngle4d(Math.PI / 2, new Vector3d(0.0, 0.0, 1.0)));
            case NORTH -> new Quaterniond(new AxisAngle4d(-Math.PI / 2, new Vector3d(0.0, 1.0, 0.0)));
            case EAST -> new Quaterniond();
            case SOUTH -> new Quaterniond(new AxisAngle4d(Math.PI / 2, new Vector3d(0.0, 1.0, 0.0)));
            case WEST -> new Quaterniond(new AxisAngle4d(Math.PI, new Vector3d(0.0, 1.0, 0.0)));
            case UP -> new Quaterniond(new AxisAngle4d(-Math.PI / 2, new Vector3d(0.0, 0.0, 1.0)));
        };
    }

    public static Quaterniond getQuaternionToEast_(Direction facing){
        return new Quaterniond(getQuaternionOfPlacement(facing))
                    .mul(new Quaterniond(new AxisAngle4d(Math.toRadians(90.0), 0.0, 0.0, 1.0)))
                    .normalize();
    }

    // the rotation to make positive-x axis and positive-y axis align with aDir and bDir respectively
    public static Quaterniond getQuaternionOfPlacement(Direction aDir, Direction bDir){
         return m2q(getRotationMatrixOfPlacement(aDir, bDir));

    }

    public static Matrix3d getRotationMatrixOfPlacement(Direction aDir, Direction bDir){
        Vector3d xDirJOML = ValkyrienSkies.set(new Vector3d(), aDir.getNormal());
        Vector3d yDirJOML = ValkyrienSkies.set(new Vector3d(), bDir.getNormal());
        Vector3d zDirJOML = new Vector3d(xDirJOML).cross(yDirJOML);

        //sc2wc
        return new Matrix3d().setRow(0, xDirJOML).setRow(1, yDirJOML).setRow(2, zDirJOML).transpose(); // wc2sc since it is transposed


    }





    // force: xq is applied
    public static Vector3d ColumnFunction(double xq, double yq, Vector3d x2y){
        double r3 = Math.pow(x2y.length(), 3);
        return new Vector3d(x2y).mul(-1e0 * xq * yq / r3);
    }

    // force: xi is applied
    public static Vector3d BiotSavartFunction(Vector3d xi, Vector3d yi, Vector3d x2y){
        double r3 = Math.pow(x2y.length(), 3);
        return new Vector3d(yi).cross(new Vector3d(xi).cross(x2y)).mul(-1e0 / r3);
    }


    // get the relative 2-D rotational speed Omega(double) of two ships (wy relative to ship_x),
    // useful when they are connected by motors
    public static double get_dyc2xc(@Nullable Ship ship_x, @Nullable Ship ship_y, Vector3dc w_x, Vector3dc w_y, Direction d_x, Direction d_y){
        Matrix3dc m_wc2xc = get_wc2sc(ship_x);
        return get_dyc2xc(m_wc2xc, w_x, w_y, d_x, d_y);
    }

    public static double get_dyc2xc(@Nullable Matrix3dc m_wc2xc, Vector3dc w_x, Vector3dc w_y, Direction d_x, Direction d_y){
        Vector3dc w_y2x_wc = w_y.sub(w_x, new Vector3d());
        Vector3dc w_y2x_xc = m_wc2xc.transform(w_y2x_wc, new Vector3d());

        int sign = (d_x == Direction.DOWN || d_x == Direction.WEST || d_x == Direction.NORTH) ? 1 : -1;
        double w_y2x =
                switch (d_x.getAxis()){
                    case X -> w_y2x_xc.x() * sign;
                    case Y -> w_y2x_xc.y() * sign;
                    case Z -> w_y2x_xc.z() * sign;
                };

        return w_y2x;

    }


    public static double clamp(double x, double threshold){
        if(x > threshold)return threshold;
        if(x < - threshold)return -threshold;
        return x;
    }

    public static double clamp0(double x, double threshold){
        if(x > threshold)return 0;
        if(x < - threshold)return 0;
        return x;
    }

    public static Matrix3d get_wc2sc(@Nullable Ship ship){
        if(ship == null)return new Matrix3d();
        return ship.getTransform().getWorldToShip().get3x3(new Matrix3d());
    }

    public static Matrix3d get_sc2wc(@Nullable Ship ship){
        if(ship == null)return new Matrix3d();
        return ship.getTransform().getShipToWorld().get3x3(new Matrix3d());
    }

    public static Matrix3d get_yc2xc(@Nullable Ship ship_x, @Nullable Ship ship_y){
        Matrix3d wc2sc_x = get_wc2sc(ship_x);
        Matrix3d wc2sc_y = get_wc2sc(ship_y);
        return get_yc2xc(wc2sc_x, wc2sc_y);
    }


    // transform vector represented by yc-basis to be represented by xc-basis
    public static Matrix3d get_yc2xc(Matrix3dc wc2sc_x, Matrix3dc wc2sc_y){
        return new Matrix3d(wc2sc_x).mul(new Matrix3d(wc2sc_y).transpose());
    }

    public static double get_yc2xc(Matrix3dc wc2sc_x, Matrix3dc wc2sc_y, Direction direction){
        Matrix3d m = get_yc2xc(wc2sc_x, wc2sc_y);
        return get_yc2xc(m, direction);
    }

    public static double get_yc2xc(Matrix3dc yc2xc, Direction direction){
        Direction.Axis axis = direction.getAxis();
        double sign = (direction == Direction.UP || direction == Direction.WEST || direction == Direction.NORTH) ? -1 : 1;
        if(axis == Direction.Axis.X){ // rotating around x-axis
            return Math.atan2(yc2xc.m21(), yc2xc.m22()) * sign; // z.y / z.z
        }
        if (axis == Direction.Axis.Y){ // rotating around y-axis
            return Math.atan2(yc2xc.m20(), yc2xc.m22()) * sign; // z.x / z.z
        }
        if (axis == Direction.Axis.Z){ // rotating around z-axis
            return Math.atan2(yc2xc.m10(), yc2xc.m11()) * sign; // y.x / y.y
        }
        return 0;
    }

    public static double get_yc2xc(Matrix3dc yc2xc, Direction xDir, Direction yDir){
        //yc2xc = yc2xc.transpose(new Matrix3d());
        int[] shuffle_1 = {2, 0, 1}; // z->y->x
        int[] shuffle_2 = {1, 2, 0}; // z->x->y
        Direction.Axis axis_x = xDir.getAxis();
        Direction.Axis axis_y = yDir.getAxis();
        int sign_0 = (xDir == Direction.DOWN || xDir == Direction.WEST || xDir == Direction.NORTH) ? -1 : 1;

        int s_axis_y0 = shuffle_1[axis_y.ordinal()];
        int s_axis_x1 = shuffle_2[axis_x.ordinal()];
        int s_axis_x2 = shuffle_1[axis_x.ordinal()];


        double Y = yc2xc.get(s_axis_y0, s_axis_x1);
        double X = yc2xc.get(s_axis_y0, s_axis_x2);
        return -sign_0 * Math.atan2(X, Y);

    }


    public static double get_yc2xc(Matrix3dc wc2sc_x, Matrix3dc wc2sc_y, Direction xDir, Direction yDir){
        return get_yc2xc(get_yc2xc(wc2sc_x, wc2sc_y), xDir, yDir);
    }

    public static Matrix3d q2m(Quaterniondc q){
        return q.get(new Matrix3d());
    }

    public static Quaterniond m2q(Matrix3dc m) {
        double trace = m.m00() + m.m11() + m.m22();
        double qw, qx, qy, qz;

        if (trace > 0) {
            double s = Math.sqrt(trace + 1.0) * 2; // s=4*qw
            qw = 0.25 * s;
            qx = (m.m21() - m.m12()) / s;
            qy = (m.m02() - m.m20()) / s;
            qz = (m.m10() - m.m01()) / s;
        } else if ((m.m00() > m.m11()) && (m.m00() > m.m22())) {
            double s = Math.sqrt(1.0 + m.m00() - m.m11() - m.m22()) * 2; // s=4*qx
            qw = (m.m21() - m.m12()) / s;
            qx = 0.25 * s;
            qy = (m.m01() + m.m10()) / s;
            qz = (m.m02() + m.m20()) / s;
        } else if (m.m11() > m.m22()) {
            double s = Math.sqrt(1.0 + m.m11() - m.m00() - m.m22()) * 2; // s=4*qy
            qw = (m.m02() - m.m20()) / s;
            qx = (m.m01() + m.m10()) / s;
            qy = 0.25 * s;
            qz = (m.m12() + m.m21()) / s;
        } else {
            double s = Math.sqrt(1.0 + m.m22() - m.m00() - m.m11()) * 2; // s=4*qz
            qw = (m.m10() - m.m01()) / s;
            qx = (m.m02() + m.m20()) / s;
            qy = (m.m12() + m.m21()) / s;
            qz = 0.25 * s;
        }

        Quaterniond quaternion = new Quaterniond(qx, qy, qz, qw);
        quaternion.normalize();
        return quaternion;
    }

    public static double get_yc2xc(@Nullable Ship ship_x, @Nullable Ship ship_y, Direction direction){
        Matrix3d m = get_yc2xc(ship_x, ship_y);
        return get_yc2xc(m, direction);

    }

    public static double get_yc2xc(Ship ship_x, Ship ship_y, Direction xDir, Direction yDir){
        Matrix3d wc2sc_x = get_wc2sc(ship_x);
        Matrix3d wc2sc_y = get_wc2sc(ship_y);
        return get_yc2xc(wc2sc_x, wc2sc_y, xDir, yDir);
    }


    public static double get_y2x(Ship ship_x, Ship ship_y, Vector3dc loc_sc_x, Vector3dc loc_sc_y, Direction direction){
        Matrix4dc s2w_x = ship_x.getTransform().getShipToWorld();
        Matrix4dc w2s_x = ship_x.getTransform().getWorldToShip();
        Matrix4dc s2w_y = ship_y.getTransform().getShipToWorld();

        Vector3dc x_wc = s2w_x.transformPosition(loc_sc_x, new Vector3d());
        Vector3dc y_wc = s2w_y.transformPosition(loc_sc_y, new Vector3d());
        Vector3dc sub_sc = w2s_x
                .transformDirection(
                        y_wc.sub(x_wc, new Vector3d()), new Vector3d()
                );


        double sign = direction.getAxisDirection() == Direction.AxisDirection.POSITIVE ? 1 : -1;
        double distance = switch (direction.getAxis()){
            case X -> sign * sub_sc.x();
            case Y -> sign * sub_sc.y();
            case Z -> sign * sub_sc.z();
        };
        return distance;
    }

    public static double get_dy2x_sc(Ship ship_x, Ship ship_y, Direction direction){
        Matrix4dc w2s_x = ship_x.getTransform().getWorldToShip();

        Vector3dc vx_wc = ship_x.getVelocity();
        Vector3dc vy_wc = ship_y.getVelocity();
        Vector3dc sub_sc = w2s_x.transformDirection(
                vy_wc.sub(vx_wc, new Vector3d())
        );
        double sign = direction.getAxisDirection() == Direction.AxisDirection.POSITIVE ? 1 : -1;
        double velocity = switch (direction.getAxis()){
            case X -> sign * sub_sc.x();
            case Y -> sign * sub_sc.y();
            case Z -> sign * sub_sc.z();
        };
        return velocity;
    }

    public static double radErrFix(double radErr){
        if(radErr > Math.PI){
            return radErr - 2 * Math.PI;
        }
        if(radErr < -Math.PI){
            return radErr + 2 * Math.PI;
        }
        return radErr;
    }


    public static Vector3d safeNormalize(Vector3d v){
        if(v.lengthSquared() > 1e-6)return new Vector3d(v).normalize();
        return v;
    }

    public static Vector3dc clamp(Vector3dc v, double threshold){
        Vector3d vc = new Vector3d(v);
        vc.x = clamp(vc.x, threshold);
        vc.y = clamp(vc.y, threshold);
        vc.z = clamp(vc.z, threshold);
        return vc;
    }

    public static Vector3d clamp(Vector3d v, double threshold){
        v.x = clamp(v.x, threshold);
        v.y = clamp(v.y, threshold);
        v.z = clamp(v.z, threshold);
        return v;
    }

    public static Quaterniond rotationToAlign(Direction staticFace, Direction dynamicFace){
        return new Quaterniond(getQuaternionOfPlacement(staticFace.getOpposite())).mul(new Quaterniond(getQuaternionOfPlacement(dynamicFace)).conjugate()).normalize();
    }

    public static boolean isVertical(Direction a, Direction b){
        return a != b.getOpposite() && a != b;
    }

    public static Quaterniond rotationToAlign(Direction align_x, Direction forward_x, Direction align_y, Direction forward_y){
        if(!(isVertical(align_x, forward_x) && isVertical(align_y, forward_y)))return new Quaterniond();
        Matrix3d m_x = getRotationMatrixOfPlacement(align_x.getOpposite(), forward_x).transpose();
        Matrix3d m_y = getRotationMatrixOfPlacement(align_y, forward_y).transpose();
        Matrix3d y2x = new Matrix3d(m_x.transpose()).mul(new Matrix3d(m_y));
        return m2q(y2x.transpose());
    }

    // I just can not figure out a formula for constrain of lock mode
    // Now I just list out all possibilities of 6 dir to 6 dir

    static List<List<Integer>> table = List.of(
            List.of(1, 3, 2, 0, 1, 1),  //down
            List.of(1, 3, 2, 0, 1, 1),
            List.of(0, 2, 1, 3, 0, 0),
            List.of(0, 2, 1, 3, 0, 0),
            List.of(3, 1, 0, 2, 3, 3),
            List.of(1, 3, 2, 0, 1, 1)
    );

    static List<Double> fixes = List.of(0.0, Math.PI / 2, Math.PI, -Math.PI / 2);


    public static double getDumbFixOfLockMode(Direction dir_x, Direction dir_y){
        int i_x = dir_x.ordinal() % 6;
        int i_y = dir_y.ordinal() % 6; // just for safety

        return fixes.get(table.get(i_y).get(i_x));
    }


    public static Vector3d getFaceCenterPos(Level level, BlockPos pos, Direction dir){
        Ship xShip = ValkyrienSkies.getShipManagingBlock(level, pos);
        Vector3d xFace_sc = getFaceCenterPosNoTransform(pos, dir);
        if(xShip == null)return xFace_sc;
        Vector3d xFace_wc = xShip.getTransform().getShipToWorld().transformPosition(xFace_sc, new Vector3d());
        return xFace_wc;
    }

    public static Vector3d getFaceCenterPosNoTransform(BlockPos pos, Direction dir){
        Vector3d xCenterJOML = ValkyrienSkies.set(new Vector3d(), pos.getCenter());
        Vector3d xDirJOML = ValkyrienSkies.set(new Vector3d(), dir.getNormal());
        Vector3d xFace_sc = xCenterJOML.fma(0.5, xDirJOML);
        return xFace_sc;
    }

}
