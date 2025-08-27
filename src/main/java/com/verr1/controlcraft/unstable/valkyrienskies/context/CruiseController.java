package com.verr1.controlcraft.unstable.valkyrienskies.context;

import com.verr1.controlcraft.unstable.ai.api.IAirController;
import com.verr1.controlcraft.unstable.ai.game.cruiser.CruiseActions;
import com.verr1.controlcraft.utils.MathUtils;
import org.joml.*;

import java.lang.Math;

import static com.verr1.controlcraft.unstable.valkyrienskies.controls.AIControlUtils.projection;
import static com.verr1.controlcraft.utils.MathUtils.safeNormalize;


import static com.verr1.controlcraft.utils.MathUtils.*;

public class CruiseController implements IAirController {

    double velocity = 30;
    double radius = 20;

    CruiseActions action = CruiseActions.VIEW;


    final Vector3d actualVelocity = new Vector3d();
    final Quaterniond actualQuaternion = new Quaterniond();
    final Vector3d targetDirection = new Vector3d();

    private static final double ts = 0.016667;

    double twistOmega = 20;
    double yawOmega = 0.5;
    double currentTurnRate = 0;

    double p_drive = 1;
    double i_drive = 0.5;

    double maxI = 50;
    double maxP = 10;
    double accumulatedDrive = 0;



    double turnResistance = 4;

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

    public Vector3dc currentDirection(){
        return targetDirection;
    }

    public void next(double rotate, double twist){

        Quaterniond next = new Quaterniond(pose);
        next
                .rotateZ(twist)
                .rotateX(rotate)
                ;
        overrideNext(next);
    }

    public double pDrive(){
        return p_drive;
    }

    public double iDrive(){
        return i_drive;
    }

    public void setPDrive(double p_drive){
        this.p_drive = p_drive;
    }

    public void setIDrive(double i_drive){
        this.i_drive = i_drive;
    }

    public double turnResistance() {
        return turnResistance;
    }

    public void setTurnResistance(double turnResistance) {
        this.turnResistance = turnResistance;
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

    double pCom = 1;
    double pYaw = 3;
    double pPitch = 8;
    double pAgRoll = 15;
    double pLvRoll = 3;
    double aTol = 20;

    public double aTol() {
        return aTol;
    }

    public void setATol(double aTol) {
        this.aTol = aTol;
    }

    public double pLvRoll() {
        return pLvRoll;
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

    public double pCom() {
        return pCom;
    }

    public void setPCom(double p) {
        this.pCom = p;
    }

    public void nextView(){
        Quaterniond q_tar = new Quaterniond().lookAlong(targetDirection, new Vector3d(0, 1, 0)).conjugate();

        lerpQuaternion.set(MathUtils.nonNan(lerpQuaternion.slerp(q_tar, 0.08).normalize()));
        Vector3dc nextView_wc = lerpQuaternion.transform(new Vector3d(0, 0, -1));
        Vector3dc nextView_sc = new Quaterniond(actualQuaternion).conjugate().transform(nextView_wc, new Vector3d()).normalize();
        double p = pCom;
        double yaw = pYaw * p * nextView_sc.x();
        double pitch = -pPitch * p * nextView_sc.y();
        double ag_roll = -pAgRoll * p * nextView_sc.x();
        double lv_roll = -pLvRoll * p * actualQuaternion.transform(new Vector3d(1, 0, 0)).y();
        double angle = nextView_sc.angle(new Vector3d(0, 0, 1));
        double rate = MathUtils.clamp(MathUtils.reverseLerp(0, Math.toRadians(aTol), angle), 0, 1);
        double roll = MathUtils.lerp(rate, lv_roll, ag_roll);


        double clampedTwist = MathUtils.clamp(roll, twistOmega * speedGain());
        double clampedYaw = MathUtils.clamp(yaw, yawOmega * speedGain());
        double clampedPitch = MathUtils.clamp(pitch, velocity / radius * speedGain());

        currentTurnRate = Math.sqrt(
                clampedPitch * clampedPitch +
                clampedYaw * clampedYaw +
                clampedTwist * clampedTwist + 1e-8
        );

        Quaterniond next = new Quaterniond(pose)
                .rotateZ(clampedTwist * ts)
                .rotateX(clampedPitch * ts)
                .rotateY(clampedYaw * ts)
                .normalize();

        overrideNext(next);
    }

    public void nextDrive(){
        double err = velocity - actualVelocity.length();
        accumulatedDrive = MathUtils.clamp(accumulatedDrive + ts * err, -maxI, maxI);
    }

    public double currentZDrive(){
        double err = MathUtils.clamp(velocity - actualVelocity.length(), -maxP, maxP);
        return p_drive * err + i_drive * accumulatedDrive;
    }

    public double currentTurnRate(){
        return currentTurnRate;
    }

    public double speedGain(){
        return actualVelocity.length() / velocity; //1; //1; //
    }

    public double inducedResistance(){
        return turnResistance * currentTurnRate();
    }

    public void overridePhysics(Vector3dc actualVelocity, Quaterniondc actualQuaternion){
        this.actualVelocity.set(actualVelocity);
        this.actualQuaternion.set(actualQuaternion);
    }

    public void nextAway(){
        Quaterniondc q = new Quaterniond(actualQuaternion);
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
        this.pose.set(MathUtils.nonNan(nextPose));
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
