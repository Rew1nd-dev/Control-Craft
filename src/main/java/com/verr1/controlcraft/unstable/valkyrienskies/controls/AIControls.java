package com.verr1.controlcraft.unstable.valkyrienskies.controls;

import com.verr1.controlcraft.foundation.vsapi.PhysShipWrapper;
import com.verr1.controlcraft.unstable.valkyrienskies.context.LogicalDirectionTarget;
import com.verr1.controlcraft.unstable.valkyrienskies.context.LogicalLerpPathTarget;
import com.verr1.controlcraft.unstable.valkyrienskies.context.LogicalPathTarget;
import com.verr1.controlcraft.unstable.valkyrienskies.context.LogicalTarget;
import com.verr1.controlcraft.utils.MathUtils;
import com.verr1.controlcraft.utils.VSMathUtils;
import org.joml.*;

import java.util.List;

import static com.verr1.controlcraft.utils.MathUtils.safeNormalize;

public class AIControls {

    public static void constantPositionCruise(PhysShipWrapper ship, LogicalTarget target){
        if(target.target() == null)return;

        Vector3dc vc_wc = ship.getVelocity();
        Vector3dc pc_wc = ship.getTransform().getPositionInWorld();
        Vector3dc vt_wc = safeNormalize(target.target().sub(pc_wc, new Vector3d())).mul(target.velocity());
        Vector3dc err_v = vt_wc.sub(vc_wc, new Vector3d());
        double mass = ship.getMass();

        double distance = pc_wc.distance(target.target());
        boolean arrived = distance < 2;

        Vector3dc force = err_v.mul(mass, new Vector3d());
        ship.applyInvariantForce(force);
    }

    public static void constantDirectionCruise(PhysShipWrapper ship, LogicalTarget target){
        if(target.target() == null)return;

        Vector3dc vc_wc = ship.getVelocity();
        Vector3dc vt_wc = safeNormalize(target.target()).mul(target.velocity());
        Vector3dc err_v = vt_wc.sub(vc_wc, new Vector3d());
        double mass = ship.getMass();


        Vector3dc force = err_v.mul(mass, new Vector3d());
        ship.applyInvariantForce(force);
    }


    public static void hardSnapControl(PhysShipWrapper ship, LogicalLerpPathTarget target){

        Vector3dc pp = target.controller().peekPrev();
        Vector3dc pt = target.controller().peekCurrent();
        Vector3dc pn = target.controller().peekNext();

        // if (pt == null)return;

        Vector3dc vc_wc = ship.getVelocity();
        Vector3dc pc_wc = ship.getTransform().getPositionInWorld();
        // if(pc_wc.distance(pt) > 50)return;

        target.controller().next();

        double P = 100;
        double D = 20;
        double mass = ship.getMass();

        Vector3dc err_p = pt.sub(pc_wc, new Vector3d());
        Vector3dc err_v = vc_wc.mul(-1.0, new Vector3d());

        Vector3dc f = err_p.mul(P * mass, new Vector3d()).add(err_v.mul(D * mass, new Vector3d()));

        Vector3dc pe = pn.sub(AIControlUtils.pedal(pt, pp, pn), new Vector3d());


        Vector3dc tz = safeNormalize(vc_wc, new Vector3d(0, 0, 1)); //pt.sub(pp, new Vector3d())

        Vector3dc __right = safeNormalize(tz.cross(new Vector3d(0, 1, 0), new Vector3d()), new Vector3d(1, 0, 0));
        Vector3dc __up = safeNormalize(__right.cross(tz, new Vector3d()), new Vector3d(0, 1, 0));

        Vector3dc $_curve_right = pe.sub(AIControlUtils.projection(pe, tz), new Vector3d());
        Vector3dc __curve_right = safeNormalize($_curve_right, __right);

        Vector3dc ty = __curve_right; //__up;// MathUtils.safeNormalize(pe, __up);
        Vector3dc tx = safeNormalize(tz.cross(ty, new Vector3d())).mul(-1);

        Matrix3dc mt = new Matrix3d().setColumn(0, tx).setColumn(1, ty).setColumn(2, tz);
        Quaterniondc qt = VSMathUtils.m2q(mt).conjugate();

        target.poseController().overrideTarget(MathUtils.nonNan(qt));

        Vector3dc t = target.poseController().calcControlTorque();

        target.poseController().overridePhysics(ship);

        ship.applyInvariantForce(MathUtils.nonNan(f));
        ship.applyInvariantTorque(MathUtils.nonNan(t));
    }


    public static void rotateControl(PhysShipWrapper ship, LogicalDirectionTarget target){

        double mass = ship.getMass();
        double targetV = target.controller().velocity();
        // Vector3dc pc_wc = ship.getTransform().getPositionInWorld();
        Vector3dc vc_wc = ship.getVelocity();

        Vector3dc vc_sc = ship.getTransform().getWorldToShip().transformDirection(vc_wc, new Vector3d());
        Vector3dc accel_xy = vc_sc.mul(-10, new Vector3d()).setComponent(2, 0);
        Vector3dc accel_z = new Vector3d(0, 0, (targetV - vc_sc.z()) * 30);
        Vector3dc f_sc = accel_z.add(accel_xy, new Vector3d()).mul(mass);
        Vector3dc f = ship.getTransform().getShipToWorld().transformDirection(f_sc, new Vector3d());

        Quaterniondc qt = target.controller().calcPose();
        target.controller().next();

        target.poseController().overrideTarget(MathUtils.nonNan(qt));

        Vector3dc t = target.poseController().calcControlTorque();

        target.poseController().overridePhysics(ship);

        if(target.disableDirectControl()){
            target.controller().overrideNext(ship.getTransform().getShipToWorldRotation());
            return;
        }

        ship.applyInvariantForce(MathUtils.nonNan(f));
        ship.applyInvariantTorque(MathUtils.nonNan(t));
    }

    private static Vector3dc tooSmallOrElse(Vector3dc v, Vector3dc o){
        return v.lengthSquared() < 1e-6 ? o : v;
    }

    public static void routeCruiseControl(PhysShipWrapper ship, LogicalPathTarget target){
        Vector3dc ps = ship.getTransform().getPositionInWorld();
        Vector3dc vs = ship.getVelocity();
        double mass = ship.getMass();
        double vt = target.speed();
        List<Vector3dc> points = target.wayPoints();
        if (points.size() <= 2)return;

        int minIndex = AIControlUtils.closestIndex(points, ps);
        int ia = minIndex == points.size() - 1 ? minIndex - 1: minIndex;
        int ib = ia + 1;


        Vector3dc pa = points.get(ia);
        Vector3dc pb = points.get(ib);
        Vector3dc pc = ib == points.size() - 1 ? pb.sub(pa, new Vector3d()).add(pb) : points.get(ib + 1);
        Vector3dc pedal = AIControlUtils.pedal(pa, pb, ps);

        Vector3dc r_p_pedal = pedal.sub(ps, new Vector3d());
        Vector3dc r_tangent = pb.sub(pa, new Vector3d());
        Vector3dc r_quad    = r_p_pedal.cross(r_tangent, new Vector3d());
        Vector3dc vsr = AIControlUtils.projection(vs, r_p_pedal);
        Vector3dc vst = AIControlUtils.projection(vs, r_tangent);
        Vector3dc vsq = AIControlUtils.projection(vs, r_quad);

        double d_p_pedal = r_p_pedal.lengthSquared();
        double vsr_scale = vsr.length();
        double vst_scale = vst.length();
        double vsq_scale = vsq.length();
        Vector3dc vsr_norm = safeNormalize(r_p_pedal);
        Vector3dc vst_norm = safeNormalize(r_tangent);
        Vector3dc vsq_norm = safeNormalize(vsq);


        double fr = (8 * d_p_pedal - 6 * vsr_scale) * mass;
        double ft = 10 * (vt - vst_scale) * mass;
        double fq = -1 * vsq_scale * mass;

        double radius = AIControlUtils.circumcircleRadius(pa, pb, pc);
        Vector3dc d_c_pedal = safeNormalize(AIControlUtils.pedal(pa, pb, pc).sub(pc));
        double fe = radius > 1e-4 ? vst_scale * vst_scale / radius : 0;

        Vector3dc _fr = vsr_norm.mul(fr, new Vector3d());
        Vector3dc _ft = vst_norm.mul(ft, new Vector3d());
        Vector3dc _fq = vsq_norm.mul(fq, new Vector3d());
        Vector3dc _fe = d_c_pedal.mul(fe, new Vector3d());

        ship.applyInvariantForce(_fr);
        ship.applyInvariantForce(_ft);
        ship.applyInvariantForce(_fq);
        ship.applyInvariantForce(_fe);

    }


}
