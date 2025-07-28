package com.verr1.controlcraft.unstable.valkyrienskies.context;

import com.verr1.controlcraft.unstable.pathing.LerpPathV2;
import com.verr1.controlcraft.utils.MathUtils;
import org.jetbrains.annotations.Nullable;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.Optional;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;

public class CruiserController {

    private final Queue<Vector3dc> wayPoints = new ConcurrentLinkedQueue<>();


    private final Vector3d prev = new Vector3d();
    private final Vector3d current = new Vector3d();
    private final Vector3d next = new Vector3d();

    double y_variant = 0;

    private final Vector3d currentGoal = new Vector3d();
    private final Vector3d currentGoalDirection = new Vector3d();

    public double speed = 40;

    public void reset(){
    }

    public int remain(){
        return wayPoints.size();
    }

    public @Nullable Vector3dc current(){
        return current;
    }

    public Vector3dc currentGoal() {
        return currentGoal;
    }

    public Vector3dc currentGoalDirection() {
        return currentGoalDirection;
    }

    public void next(){
        prev.set(current);
        current.set(next);
        next.set(Optional.ofNullable(wayPoints.poll()).orElse(next));
    }

    public void offerPath(@Nullable LerpPathV2<Vector3dc> wayPoints){
        if(wayPoints == null)return;
        double y_var_range = 20;
        Vector3d prevPoint = new Vector3d(wayPoints.point(0));
        Vector3d newPoint = new Vector3d(wayPoints.point(0));
        double ratio = Math.sqrt(1.5 - y_variant / y_var_range);
        for (double d = speed / 60 * ratio; d < wayPoints.length(); ){
            prevPoint.set(newPoint);
            newPoint.set(wayPoints.point(d));
            y_variant = MathUtils.clamp(y_variant + newPoint.y - prevPoint.y, -y_var_range, y_var_range );
            this.wayPoints.offer(new Vector3d(newPoint));

            ratio = Math.sqrt(1.5 - y_variant / y_var_range) ;
            d += speed / 60 * ratio;
        }
        currentGoal.set(wayPoints.end());
        Vector3dc goalPrev = wayPoints.point(wayPoints.length() - speed / 60);
        currentGoalDirection.set(currentGoal.sub(goalPrev, new Vector3d()));

        reset();
    }

    public Vector3dc peekNext(){
        return next;
    }

    public Vector3dc peekPrevious(){
        return prev;
    }

}
