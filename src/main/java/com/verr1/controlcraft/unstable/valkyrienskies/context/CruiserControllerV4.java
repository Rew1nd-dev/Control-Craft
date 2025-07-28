package com.verr1.controlcraft.unstable.valkyrienskies.context;

import com.verr1.controlcraft.unstable.ai.game.cruiser.CruiseActions;
import org.joml.*;

import java.lang.Math;

import static com.verr1.controlcraft.unstable.valkyrienskies.controls.AIControlUtils.projection;
import static com.verr1.controlcraft.utils.MathUtils.safeNormalize;


import static com.verr1.controlcraft.utils.MathUtils.*;

public class CruiserControllerV4 {

    double velocity = 30;
    double radius = 20;



    CruiseActions action = CruiseActions.TOWARDS;
    final Vector3d targetDirection = new Vector3d();

    double twistOmega = 20;
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

    public double twistOmega() {
        return twistOmega;
    }

    public void setTwistOmega(double twistOmega) {
        this.twistOmega = twistOmega;
    }

    public void next(){
        switch (action){
            case AWAY -> nextAway();
            case TOWARDS -> nextTowards();
            case TEST -> nextTest();
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

    public void nextTest(){
        Quaterniond q_tar = new Quaterniond().lookAlong(targetDirection, new Vector3d(0, 1, 0));

        Quaterniond next = new Quaterniond(pose);
        next.slerp(q_tar, 1 - Math.exp(-5 * ts));
        overrideNext(next);


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
