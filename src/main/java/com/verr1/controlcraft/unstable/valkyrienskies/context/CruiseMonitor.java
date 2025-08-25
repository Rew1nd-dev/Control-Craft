package com.verr1.controlcraft.unstable.valkyrienskies.context;

import com.verr1.controlcraft.unstable.ai.api.IAirController;
import com.verr1.controlcraft.utils.MathUtils;
import org.joml.Quaterniond;
import org.joml.Quaterniondc;
import org.joml.Vector3d;
import org.joml.Vector3dc;

public class CruiseMonitor implements IAirController {



    final Vector3d targetDirection = new Vector3d();
    final Quaterniond pose = new Quaterniond();
    double velocity = 50;
    double ts = 0.01667;

    double p = 1;
    double pYaw = 3;
    double pPitch = -8;
    double pAgRoll = -25;
    double pLvRoll = -25;
    double aTol = 10;

    public double lerpRate() {
        return lerpRate;
    }

    public void setLerpRate(double lerpRate) {
        this.lerpRate = lerpRate;
    }

    public double aTol() {
        return aTol;
    }

    public void setATol(double aTol) {
        this.aTol = aTol;
    }

    public double pLvRoll() {
        return pLvRoll;
    }

    public Vector3dc targetDirection() {
        return targetDirection;
    }

    public void setPLvRoll(double pLvRoll) {
        this.pLvRoll = pLvRoll;
    }

    public double pAgRoll() {
        return pAgRoll;
    }

    public void setPAgRoll(double pAgRoll) {
        this.pAgRoll = pAgRoll;
    }

    public double pPitch() {
        return pPitch;
    }

    public void setPPitch(double pPitch) {
        this.pPitch = pPitch;
    }

    public double pYaw() {
        return pYaw;
    }

    public void setPYaw(double pYaw) {
        this.pYaw = pYaw;
    }

    public double p() {
        return p;
    }

    public void setP(double p) {
        this.p = p;
    }

    double lerpRate = 5;

    double yawControl = 0;
    double pitchControl = 0;

    public double rollControl() {
        return rollControl;
    }

    public double pitchControl() {
        return pitchControl;
    }

    public double yawControl() {
        return yawControl;
    }

    double rollControl = 0;

    @Override
    public void overrideTarget(Vector3dc target) {
        targetDirection.set(target);
    }



    @Override
    public void setVelocity(double v) {
        velocity = v;
    }


    public void overridePose(Quaterniondc q){
        pose.set(q);
    }

    private final Quaterniond lerpQuaternion = new Quaterniond();
    public void nextView(){
        Quaterniond q_tar = new Quaterniond().lookAlong(targetDirection, new Vector3d(0, 1, 0)).conjugate();

        lerpQuaternion.slerp(q_tar, 1 - Math.exp(-Math.abs(lerpRate) * ts)).normalize();
        Vector3dc nextView_wc = lerpQuaternion.transform(new Vector3d(0, 0, -1));
        Vector3dc nextView_sc = new Quaterniond(pose).conjugate().transform(nextView_wc, new Vector3d()).normalize();
        yawControl = pYaw * p * nextView_sc.x();
        pitchControl = pPitch * p * nextView_sc.y();
        double ag_roll = pAgRoll * p * nextView_sc.x();
        double lv_roll = pLvRoll * p * pose.transform(new Vector3d(1, 0, 0)).y();
        double angle = nextView_sc.angle(new Vector3d(0, 0, 1));
        double rate = MathUtils.clamp(MathUtils.reverseLerp(0, Math.toRadians(aTol), angle), 0, 1);
        rollControl = MathUtils.lerp(rate, lv_roll, ag_roll);



    }

}
