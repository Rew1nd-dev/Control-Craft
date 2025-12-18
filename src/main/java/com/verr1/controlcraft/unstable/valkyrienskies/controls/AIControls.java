package com.verr1.controlcraft.unstable.valkyrienskies.controls;

import com.verr1.controlcraft.unstable.valkyrienskies.context.LogicalDirectionTarget;
import com.verr1.controlcraft.utils.MathUtils;
import org.joml.*;
import org.valkyrienskies.core.api.ships.PhysShip;

import static com.verr1.controlcraft.utils.MathUtils.safeNormalize;

public class AIControls {


    public static void rotateControl(PhysShip ship, LogicalDirectionTarget target){

        double mass = ship.getMass();
        double turnRate = target.controller().inducedResistance();
        double zDrive = target.controller().currentZDrive();
        // Vector3dc pc_wc = ship.getTransform().getPositionInWorld();
        Vector3dc vc_wc = ship.getVelocity();

        Vector3dc vc_sc = ship.getTransform().getWorldToShip().transformDirection(vc_wc, new Vector3d());
        Vector3dc accel_xy = vc_sc.mul(-30, new Vector3d()).setComponent(2, 0);
        Vector3dc accel_z = new Vector3d(0, 0, zDrive - turnRate);
        Vector3dc f_sc = accel_z.add(accel_xy, new Vector3d()).mul(mass);
        Vector3dc f = ship.getTransform().getShipToWorld().transformDirection(f_sc, new Vector3d());

        Quaterniondc qt = target.controller().calcPose();
        target.controller().next();
        target.controller().nextDrive();

        target.poseController().overrideTarget(MathUtils.nonNan(qt));

        Vector3dc t = target.poseController().calcControlTorque();

        target.poseController().overridePhysics(ship);
        target.controller().overridePhysics(ship.getVelocity(), ship.getTransform().getShipToWorldRotation());

        ship.applyWorldForceToBodyPos(MathUtils.nonNan(f), new Vector3d());
        ship.applyWorldTorque(MathUtils.nonNan(t));
    }

    private static Vector3dc tooSmallOrElse(Vector3dc v, Vector3dc o){
        return v.lengthSquared() < 1e-6 ? o : v;
    }




}
