package com.verr1.controlcraft.unstable.valkyrienskies.context;

import com.verr1.controlcraft.unstable.valkyrienskies.controls.AIControlUtils;
import com.verr1.controlcraft.utils.MathUtils;
import com.verr1.controlcraft.utils.VSMathUtils;
import org.joml.*;

import java.lang.Math;

import static com.verr1.controlcraft.unstable.valkyrienskies.controls.AIControlUtils.*;
import static com.verr1.controlcraft.utils.MathUtils.*;

public class CruiserControllerV3 {

    final Vector3d targetDirection = new Vector3d();
    final Vector3d targetVelocity = new Vector3d();



    double velocity = 30;




    double radius = 20;
    double ts = 0.01667;

    final Vector3d currentDelta = new Vector3d(0, 0, 1);
    final Vector3d previousDelta = new Vector3d(0, 0, 1);

    final Vector3d latestX = new Vector3d(1, 0, 0);

    public void next0(){
        double angle = velocity / radius * ts;
        if(targetDirection.length() < 1e-2)return;
        if(Math.abs(targetDirection.angle(currentDelta)) < angle)return;
        Vector3dc tangent = safeNormalize(currentDelta.cross(targetDirection, new Vector3d()));
        Vector3dc next = currentDelta.rotateAxis(angle, tangent.x(), tangent.y(), tangent.z(), new Vector3d());
        overrideNext(safeNormalize(next));
    }

    public void setRadius(double radius) {
        this.radius = radius;
    }


    boolean selectOne = false;

    public void setEscaping(boolean isEscaping){
        selectOne = isEscaping;
    }

    public void next1(){
        double angle = velocity / radius * ts;
        if(targetDirection.length() < 1e-2)return;
        if(Math.abs(targetDirection.angle(currentDelta)) < angle)return;

        Vector3dc currentVelocity = currentDelta.mul(1 / ts, new Vector3d());
        Vector3dc targetRefVelocity = targetVelocity.sub(currentVelocity, new Vector3d());
        // Vector3dc targetProjRefVelocity = AIControlUtils.projection(targetRefVelocity, targetDirection, currentVelocity);

        Vector3dc tangent = safeNormalize(currentDelta.cross(targetDirection, new Vector3d(0, 1, 0)));

        Vector3dc currentVelocityNext_1 = currentDelta.rotateAxis( angle, tangent.x(), tangent.y(), tangent.z(), new Vector3d());
        Vector3dc currentVelocityNext_2 = currentDelta.rotateAxis(-angle, tangent.x(), tangent.y(), tangent.z(), new Vector3d());

        Vector3dc center = circumcenter(currentDeltaPosition(), previousDeltaPosition(), new Vector3d());



        Vector3dc targetDirectionNext = targetDirection.fma(ts, targetRefVelocity, new Vector3d());
        double angle_1 = Math.abs(currentVelocity.angle(targetDirection));
        double angle_2 = 2 * Math.PI - Math.abs(currentVelocity.angle(targetDirection));

        double angle_next_1 = Math.abs(currentVelocityNext_1.angle(targetDirectionNext));
        double angle_next_2 = 2 * Math.PI - Math.abs(currentVelocityNext_2.angle(targetDirectionNext));

        double ratio_1 = -(angle_next_1 - angle_1) / angle_1;
        double ratio_2 = -(angle_next_2 - angle_2) / angle_2;


        boolean twoIsBetter = ratio_1 < ratio_2 && Math.abs(ratio_1 - ratio_2) > 0.01; // && Math.abs(ratio_1 - ratio_2) / (ratio_1 + 1e-8) > 0.1

        Vector3dc next = !twoIsBetter || selectOne ? //center.distance(targetDirection) > radius ||
                currentVelocityNext_1 :
                currentVelocityNext_2;

        overrideNext(safeNormalize(next));
    }

    public void next(){
        double angle = velocity / radius * ts;
        double twist = velocity / radius * ts;
        if(targetDirection.length() < 1e-2)return;
        if(Math.abs(targetDirection.angle(currentDelta)) < angle)return;

        Vector3dc rotTang_choice = new Vector3d(0, 1, 0).cross(targetVelocity);
        Vector3dc rotTang = safeNormalize(AIControlUtils.tangent(currentDelta, previousDelta), rotTang_choice);

        Vector3dc currentVelocity = currentDelta.mul(1 / ts, new Vector3d());
        Vector3dc targetRefVelocity = targetVelocity.sub(currentVelocity, new Vector3d());
        Vector3dc targetProjRefVelocity = AIControlUtils.projection(targetRefVelocity, targetDirection, currentVelocity);

        Vector3dc tangent = safeNormalize(currentDelta.cross(targetDirection, new Vector3d(0, 1, 0)));

        Vector3dc currentVelocityNext_1 = currentDelta.rotateAxis( angle, tangent.x(), tangent.y(), tangent.z(), new Vector3d());
        Vector3dc currentVelocityNext_2 = currentDelta.rotateAxis(-angle, tangent.x(), tangent.y(), tangent.z(), new Vector3d());

        Vector3dc center = circumcenter(currentDeltaPosition(), previousDeltaPosition(), new Vector3d());



        Vector3dc targetDirectionNext = targetDirection.fma(ts, targetRefVelocity, new Vector3d());
        double angle_1 = Math.abs(currentVelocity.angle(targetDirection));
        double angle_2 = 2 * Math.PI - Math.abs(currentVelocity.angle(targetDirection));

        double angle_next_1 = Math.abs(currentVelocityNext_1.angle(targetDirectionNext));
        double angle_next_2 = 2 * Math.PI - Math.abs(currentVelocityNext_2.angle(targetDirectionNext));

        double ratio_1 = -(angle_next_1 - angle_1) / angle_1;
        double ratio_2 = -(angle_next_2 - angle_2) / angle_2;

        boolean sameSize = sameSide(targetProjRefVelocity, currentVelocity, targetDirection);

        boolean twoIsBetter = ratio_1 < ratio_2 && Math.abs(ratio_1 - ratio_2) > 0.01; // && Math.abs(ratio_1 - ratio_2) / (ratio_1 + 1e-8) > 0.1

        Vector3dc next = !twoIsBetter || selectOne ? //center.distance(targetDirection) > radius ||
                currentVelocityNext_1 :
                currentVelocityNext_2;

        overrideNext(safeNormalize(next));
    }

    public void overrideTarget(Vector3dc targetDirection, Vector3dc targetVelocity){
        this.targetDirection.set(targetDirection);
        this.targetVelocity.set(targetVelocity);
    }

    public double radius() {
        return radius;
    }

    public void overrideNext(Vector3dc nextDelta){
        this.previousDelta.set(currentDelta);
        this.currentDelta.set(nextDelta);
    }

    public Vector3d previousDelta() {
        return previousDelta.mul(velocity * ts, new Vector3d());
    }

    public Vector3d currentDelta() {
        return currentDelta.mul(velocity * ts, new Vector3d());
    }

    public void setVelocity(double velocity) {
        this.velocity = velocity;
    }

    public Vector3dc previousDeltaPosition(){
        return currentDeltaPosition().sub(previousDelta());
    }

    public Vector3d currentDeltaPosition(){
        return currentDelta.mul(-1, new Vector3d());
    }

    public Vector3d nextDeltaPosition(){
        return new Vector3d();
    }

    private Vector3d fromLatestX(Vector3dc tz){
        return latestX.sub(projection(latestX, tz), new Vector3d());
    }

    public double velocity() {
        return velocity;
    }

    public Quaterniond calcPose(){
        Vector3dc tz = currentDelta;
        Vector3dc tx = safeNormalize(currentDelta.cross(previousDelta, new Vector3d()), fromLatestX(tz));
        Vector3dc ty = safeNormalize(tz.cross(tx, new Vector3d()));

        latestX.set(tx);

        Matrix3dc mt = new Matrix3d().setColumn(0, tx).setColumn(1, ty).setColumn(2, tz);
        return VSMathUtils.m2q(mt).conjugate();
    }


}
