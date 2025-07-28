package com.verr1.controlcraft.unstable.valkyrienskies.context;

import com.verr1.controlcraft.foundation.vsapi.PhysShipWrapper;
import org.joml.Quaterniond;
import org.joml.Quaterniondc;
import org.joml.Vector3d;
import org.joml.Vector3dc;

public class PoseController {

    private final Quaterniond current = new Quaterniond();
    private final Quaterniond previous = new Quaterniond();
    private final Quaterniond target = new Quaterniond();

    private final Quaterniond q_err = new Quaterniond();
    private final Quaterniond q_err_prev = new Quaterniond();

    protected double p = 24;
    protected double d = 12;

    private double mass = 1;
    private double inertia = 1;

    private double ts = 1d / 60;

    public void overridePhysics(PhysShipWrapper ship){
        mass = ship.getMass();
        inertia = ship.getMomentOfInertia().m00();

        previous.set(current);
        current.set(ship.getTransform().getShipToWorldRotation());


    }

    public void overrideTarget(Quaterniondc q_tar){
        this.target.set(q_tar);
        q_err_prev.set(q_err);
        q_err.set(new Quaterniond(target).mul(new Quaterniond(current).conjugate()));
    }

    public Vector3dc calcControlTorque(){
        Quaterniondc q_d = new Quaterniond(q_err).conjugate().mul(q_err_prev);
        double sign = q_err.w() < 0 ? -1 : 1;
        Vector3dc accel_p = new Vector3d(q_err.x(), q_err.y(), q_err.z()).mul(sign * p);
        Vector3dc accel_d = new Vector3d(q_d.x(), q_d.y(), q_d.z()).mul(-2 / ts).mul(d);
        return new Vector3d(accel_p).add(accel_d).mul(inertia);
    }

}
