package com.verr1.controlcraft.unstable.valkyrienskies.context;

import com.verr1.controlcraft.unstable.ai.api.IAirController;
import com.verr1.controlcraft.unstable.ai.game.cruiser.CruiseActions;
import com.verr1.controlcraft.utils.MathUtils;
import org.joml.*;

import java.lang.Math;

import static com.verr1.controlcraft.unstable.valkyrienskies.controls.AIControlUtils.projection;
import static com.verr1.controlcraft.utils.MathUtils.safeNormalize;


import static com.verr1.controlcraft.utils.MathUtils.*;

public class CruiserControllerV4 implements IAirController {

    double velocity = 30;
    double radius = 20;



    CruiseActions action = CruiseActions.TOWARDS;



    final Vector3d targetDirection = new Vector3d();

    double twistOmega = 20;
    double yawOmega = 0.5;
    double ts = 0.01667;

    final Quaterniond pose = new Quaterniond();

    public Vector3d rotAxis(){
        return pose.transform(new Vector3d(1, 0, 0));
    }

    public Vector3d yawAxis(){
        return pose.transform(new Vector3d(0, 1, 0));
    }

    public Vector3d twistAxis(){
        return pose.transform(new Vector3d(0, 0, 1));
    }

    public void next(double rotate, double twist){

        Quaterniond next = new Quaterniond(pose);
        next
                .rotateZ(twist)
                .rotateX(rotate)
                ;
        overrideNext(next);
    }

    public Vector3d targetDirection() {
        return targetDirection;
    }

    public double twistOmega() {
        return twistOmega;
    }

    public void setTwistOmega(double twistOmega) {
        this.twistOmega = twistOmega;
    }

    public double yawOmega(){
        return yawOmega;
    }

    public void setYawOmega(double yawOmega){
        this.yawOmega = yawOmega;
    }

    public void next(){
        switch (action){
            case AWAY -> nextAway();
            case TOWARDS -> nextTowards();
            case VIEW -> nextView();
        }
    }

    public CruiseActions action() {
        return action;
    }

    public void setAction(CruiseActions action) {
        this.action = action;
    }

    public void nextTowards(){

        Quaterniondc q = calcPose();
        Vector3dc op = q.conjugate(new Quaterniond()).transform(targetDirection, new Vector3d());
        double phi = Math.atan2(op.x(), op.y());
        double theta = op.angle(new Vector3d(0, 0, 1));
        double radius = radius();
        double velocity = velocity();
        double twistAn = twistOmega() * 0.01667;
        double rotateAn = velocity / radius * 0.01667;
        boolean shouldRot = op.y() > 0;
        boolean shouldTwist = Math.abs(theta) > 1e-1;
        double finalPhi = shouldTwist ? clamp(-phi, twistAn) : 0;
        double finalTheta = shouldRot ? clamp(-theta, rotateAn) : 0;



        next(finalTheta, finalPhi);
    }

    private final Quaterniond lerpQuaternion = new Quaterniond();




    private volatile boolean useActualFlight = false;

    private volatile double yawControl = 0;
    private volatile double pitchControl = 0;
    private volatile double rollControl = 0;

    public double yawControl() {
        return yawControl;
    }

    public double pitchControl() {
        return pitchControl;
    }

    public double rollControl() {
        return rollControl;
    }

    public boolean useActualFlight() {
        return useActualFlight;
    }

    public void setUseActualFlight(boolean useActualFlight) {
        this.useActualFlight = useActualFlight;
    }

    public void nextView(){
        Quaterniond q_tar = new Quaterniond().lookAlong(targetDirection, new Vector3d(0, 1, 0)).conjugate();

        lerpQuaternion.slerp(q_tar, 1 - Math.exp(-5 * ts)).normalize();
        Vector3dc nextView_wc = lerpQuaternion.transform(new Vector3d(0, 0, -1));
        Vector3dc nextView_sc = new Quaterniond(pose).conjugate().transform(nextView_wc, new Vector3d()).normalize();
        double p = 1;
        double yaw = 3 * p * nextView_sc.x();
        double pitch = -8 * p * nextView_sc.y();
        double ag_roll = -25 * p * nextView_sc.x();
        double lv_roll = -25 * p * pose.transform(new Vector3d(1, 0, 0)).y();
        double angle = nextView_sc.angle(new Vector3d(0, 0, 1));
        double rate = MathUtils.clamp(MathUtils.reverseLerp(0, Math.toRadians(10), angle), 0, 1);
        double roll = MathUtils.lerp(rate, lv_roll, ag_roll);

        yawControl = yaw;
        pitchControl = pitch;
        rollControl = roll;

        Quaterniond next = new Quaterniond(pose)
                .rotateZ(MathUtils.clamp(roll, twistOmega) * ts)
                .rotateX(MathUtils.clamp(pitch, velocity / radius) * ts)
                .rotateY(MathUtils.clamp(yaw, velocity / radius) * ts)
                .normalize();

        if(!useActualFlight) {
            overrideNext(next);
        }
    }


    public void nextAway(){
        Quaterniondc q = calcPose();
        Vector3dc op = q.conjugate(new Quaterniond()).transform(targetDirection, new Vector3d());
        double phi = Math.atan2(op.x(), op.y());
        double theta = op.angle(new Vector3d(0, 0, 1));
        double radius = radius();
        double velocity = velocity();
        double twistAn = twistOmega() * 0.01667;
        double rotateAn = velocity / radius * 0.01667;
        boolean shouldRot = op.y() > 0;
        boolean shouldTwist = Math.abs(theta) > 1e-1;
        double finalPhi = shouldTwist ? clamp(-phi, twistAn) : 0;
        double finalTheta = shouldRot ? clamp(-theta, rotateAn) : 0;

        next(finalTheta, finalPhi);
    }

    public void overrideTarget(Vector3dc targetDirection){
        this.targetDirection.set(targetDirection);
    }

    public void setRadius(double radius) {
        this.radius = radius;
    }


    public double radius() {
        return radius;
    }

    public void overrideNext(Quaterniondc nextPose){
        this.pose.set(nextPose);
    }



    public void setVelocity(double velocity) {
        this.velocity = velocity;
    }


    public double velocity() {
        return velocity;
    }

    public Quaterniond calcPose(){
        return new Quaterniond(pose);
    }


}
