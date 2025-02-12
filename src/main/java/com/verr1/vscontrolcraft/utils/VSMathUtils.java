package com.verr1.vscontrolcraft.utils;

import com.verr1.vscontrolcraft.base.DataStructure.LevelPos;
import com.verr1.vscontrolcraft.blocks.spinalyzer.ShipPhysics;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntity;
import org.joml.*;
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.core.api.ships.Ship;
import org.valkyrienskies.core.impl.game.ships.PhysInertia;
import org.valkyrienskies.core.impl.game.ships.PhysShipImpl;
import org.valkyrienskies.core.impl.game.ships.ShipObjectServerWorld;
import org.valkyrienskies.mod.common.VSGameUtilsKt;
import org.valkyrienskies.physics_api.PoseVel;

import javax.annotation.Nullable;
import java.lang.Math;

//XC2YC:  [X, Y, Z]: X: the X unit basis in YC coordinate represented by XC coordinate unit vector basis,
//                      such that XC2YC * v_x(represented in XC coordinate) = v_y(represented in YC coordinate)
public class VSMathUtils {


    public static Quaterniondc getQuaternionOfPlacement(Direction facing){
        return switch (facing){
            case DOWN -> new Quaterniond(new AxisAngle4d(Math.PI, new Vector3d(1.0, 0.0, 0.0)));
            case NORTH -> (new Quaterniond(new AxisAngle4d(Math.PI, new Vector3d(0.0, 1.0, 0.0)))).mul((new Quaterniond(new AxisAngle4d(Math.PI / 2, (new Vector3d(1.0, 0.0, 0.0)))))).normalize();
            case EAST -> new Quaterniond(new AxisAngle4d(Math.PI / 2,  new Vector3d(0.0, 1.0, 0.0))).mul(new Quaterniond(new AxisAngle4d(Math.PI / 2, (new Vector3d(1.0, 0.0, 0.0))))).normalize();
            case SOUTH -> (new Quaterniond(new AxisAngle4d(Math.PI / 2, (new Vector3d(1.0, 0.0, 0.0))))).normalize();
            case WEST ->(new Quaterniond(new AxisAngle4d(Math.PI * 3 / 2, (new Vector3d(0.0, 1.0, 0.0))))).mul((Quaterniondc)(new Quaterniond(new AxisAngle4d(Math.PI / 2, (new Vector3d(1.0, 0.0, 0.0)))))).normalize();
            default -> new Quaterniond();
        };
    }

    // the rotation to make positive-x axis and positive-y axis align with aDir and bDir respectively
    public static Quaterniond getQuaternionOfPlacement(Direction aDir, Direction bDir){
        Vector3d xDirJOML = Util.Vec3itoVector3d(aDir.getNormal());
        Vector3d yDirJOML = Util.Vec3itoVector3d(bDir.getNormal());
        Vector3d zDirJOML = new Vector3d(xDirJOML).cross(yDirJOML);
        //sc2wc
        Matrix3d m = new Matrix3d().setRow(0, xDirJOML).setRow(1, yDirJOML).setRow(2, zDirJOML); // wc2sc since it is transposed
        return m2q(m.transpose());

    }

    public static Matrix3d getRotationMatrixOfPlacement(Direction aDir, Direction bDir){
        Vector3d xDirJOML = Util.Vec3itoVector3d(aDir.getNormal());
        Vector3d yDirJOML = Util.Vec3itoVector3d(bDir.getNormal());
        Vector3d zDirJOML = new Vector3d(xDirJOML).cross(yDirJOML);

        //sc2wc
        return new Matrix3d().setRow(0, xDirJOML).setRow(1, yDirJOML).setRow(2, zDirJOML); // wc2sc since it is transposed


    }

    public static Vector3d getFaceCenterPos(ServerLevel level, BlockPos pos, Direction dir){
        ServerShip xShip = VSGameUtilsKt.getShipObjectManagingPos(level, pos);
        Vector3d xFace_sc = getFaceCenterPosNoTransform(pos, dir);
        if(xShip == null)return xFace_sc;
        Vector3d xFace_wc = xShip.getTransform().getShipToWorld().transformPosition(xFace_sc, new Vector3d());
        return xFace_wc;
    }

    public static Vector3d getFaceCenterPosNoTransform(BlockPos pos, Direction dir){
        Vector3d xCenterJOML = Util.Vec3toVector3d(pos.getCenter());
        Vector3d xDirJOML = Util.Vec3itoVector3d(dir.getNormal());
        Vector3d xFace_sc = xCenterJOML.fma(0.5, xDirJOML);
        return xFace_sc;
    }

    public static String getDimensionID(Level level){
        return VSGameUtilsKt.getDimensionId(level);
    }

    public static boolean isSameDimension(Level x, Level y){
        return getDimensionID(x).equals(getDimensionID(y));
    }

    public static long getServerShipID(BlockPos pos, ServerLevel level){
        ServerShip ship = getServerShip(pos, level);
        if(ship != null)return ship.getId();
        String dimensionID = getDimensionID(level);
        ShipObjectServerWorld sosw = (ShipObjectServerWorld)VSGameUtilsKt.getShipObjectWorld((ServerLevel) level);
        return sosw.getDimensionToGroundBodyIdImmutable().get(dimensionID);
    }

    public static boolean isOnServerShip(BlockPos pos, ServerLevel level){
        return getServerShip(pos, level) != null;
    }


    public static @Nullable ServerShip getServerShip(LevelPos pos){
        return getServerShip(pos.pos(), pos.level());
    }

    public static @Nullable ServerShip getServerShip(BlockPos pos, ServerLevel level){
        if(level.isClientSide)return null;
        ServerShip ship = VSGameUtilsKt.getShipObjectManagingPos(level, pos);
        return ship;
    }


    public static Quaterniondc getQuaternion(LevelPos pos){
        ServerShip xShip = VSGameUtilsKt.getShipObjectManagingPos(pos.level(), pos.pos());
        if(xShip == null)return new Quaterniond();
        Quaterniondc xBaseQuaternion = xShip.getTransform().getShipToWorldRotation();
        return xBaseQuaternion;
    }

    public static Vector3d getAbsolutePosition(BlockPos pos, ServerLevel level, Direction facing){
        ServerShip ship = getServerShip(pos, level);
        Vector3d faceCenter = getFaceCenterPosNoTransform(pos, facing);
        if(ship == null)return faceCenter;
        var faceCenter_wc = ship.getTransform().getShipToWorld().transformPosition(faceCenter, new Vector3d());
        return faceCenter_wc;
    }

    public static Vector3d getAbsolutePosition(LevelPos pos){
        return getAbsolutePosition(pos.level(), pos.pos());
    }

    public static Vector3d getAbsolutePosition(BlockEntity e){
        if(e.getLevel() != null && !e.getLevel().isClientSide){
            return getAbsolutePosition((ServerLevel) e.getLevel(), e.getBlockPos());
        }
        return new Vector3d();
    }

    public static Vector3d getAbsolutePosition(ServerLevel level, BlockPos pos){
        ServerShip ship = getServerShip(pos, level);
        Vector3d p_sc = Util.Vec3toVector3d(pos.getCenter());
        if(ship == null)return p_sc;
        var p_wc = ship.getTransform().getShipToWorld().transformPosition(p_sc, new Vector3d());
        return p_wc;
    }

    public static boolean isOnSameShip(LevelPos x, LevelPos y){
        ServerShip sx = getServerShip(x);
        ServerShip sy = getServerShip(y);
        if(sx == null || sy == null)return false;
        return sx.getId() == sy.getId();
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

    public static double radErrFix(double radErr){
        if(radErr > Math.PI){
            return radErr - 2 * Math.PI;
        }
        if(radErr < -Math.PI){
            return radErr + 2 * Math.PI;
        }
        return radErr;
    }

    public static ShipPhysics getShipPhysics(PhysShipImpl ship){
        if(ship == null)return ShipPhysics.EMPTY;
        PoseVel poseVel = ship.getPoseVel();
        PhysInertia inertia = ship.getInertia();
        return new ShipPhysics(
                new Vector3d(poseVel.getVel()),
                new Vector3d(poseVel.getOmega()),
                new Vector3d(poseVel.getPos()),
                new Quaterniond(poseVel.getRot()),
                new Matrix3d(inertia.getMomentOfInertiaTensor()),
                new Matrix3d(ship.getTransform().getShipToWorld()),
                new Matrix4d(ship.getTransform().getShipToWorld()),
                new Matrix4d(ship.getTransform().getWorldToShip()),
                inertia.getShipMass(),
                ship.getId()
        );
    }

    public static BlockEntity getExisting(LevelPos pos){
        return pos.level().getExistingBlockEntity(pos.pos());
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
        Matrix3d m_x = getRotationMatrixOfPlacement(align_x.getOpposite(), forward_x);
        Matrix3d m_y = getRotationMatrixOfPlacement(align_y, forward_y);
        Matrix3d y2x = new Matrix3d(m_x.transpose()).mul(new Matrix3d(m_y));
        return m2q(y2x.transpose());
    }
}
