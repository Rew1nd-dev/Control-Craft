package com.verr1.controlcraft.unstable.ai.game.cruiser.v1;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;
import com.verr1.controlcraft.unstable.ai.core.nodes.Action;
import com.verr1.controlcraft.unstable.blocks.cruiser.CruiserBlockEntity;
import com.verr1.controlcraft.utils.MathUtils;
import org.joml.Vector3d;
import org.joml.Vector3dc;


public class ChaseAction extends Action {

    @Override
    protected Status perform(Blackboard blackboard) {
        CruiserBlockEntity context = blackboard.get(CruiserBlockEntity.CONTEXT);

        if (context == null)return Status.FAILURE;

        // ControlCraft.LOGGER.debug("chasing");

        // context.controller().setEscaping(false);

        Vector3dc targetPNullable = context.debug_getTarget();
        Vector3dc targetVNullable = context.debug_getTargetVelocity();

        if(targetPNullable == null)return Status.FAILURE;
        Vector3dc currentP = context.getPosition();
        Vector3dc currentV = context.getVelocity();

        Vector3dc targetV = targetVNullable == null ? new Vector3d() : targetVNullable;
        Vector3dc aim = aimPredict(targetPNullable, targetV, currentP, 180);
        Vector3dc targetP = aim != null ? aim : targetPNullable;

        Vector3dc dir = targetP.sub(currentP, new Vector3d());

        // context.controller().overrideTarget(dir, targetV);

        Vector3dc front = context.readSelf().s2wTransform().transformDirection(new Vector3d(0, 0, 1));
        double angle_f = Math.abs(dir.angle(front));
        double tol = context.shootTolerance();
        if(dir.length() > 1 && angle_f < Math.toRadians(tol)){
            context.fireAt(dir);
        }
        return Status.RUNNING;
    }

    public static Vector3dc aimPredict(Vector3dc p_t, Vector3dc v_t, Vector3dc p_c, double v_b){

        Vector3d p_rel = new Vector3d(p_t).sub(p_c);

        // 计算目标速度的平方模长
        double v_t_mag_sq = v_t.lengthSquared();

        // 计算二次方程的系数
        double a = v_t_mag_sq - (v_b * v_b);
        double b = 2.0 * v_t.dot(p_rel);
        double c = p_rel.lengthSquared();

        // 计算判别式
        double delta = b * b - 4 * a * c;

        // 如果判别式小于0，无实数解
        if (delta < 0) {
            return null;
        }

        // 计算两个可能的时间解
        double sqrtDelta = Math.sqrt(delta);
        double t1 = (-b + sqrtDelta) / (2 * a);
        double t2 = (-b - sqrtDelta) / (2 * a);

        // 寻找最小的正时间解
        double t_hit = Double.POSITIVE_INFINITY;
        if (t1 > 0) t_hit = t1;
        if (t2 > 0 && t2 < t_hit) t_hit = t2;

        // 如果没有有效解
        if (t_hit == Double.POSITIVE_INFINITY) {
            return null;
        }

        // 计算目标在命中时刻的位置: p_t + v_t * t_hit
        return new Vector3d(v_t).mul(t_hit).add(p_t);
    }

    public static class CruiseState{
        double yVariant = 0;
        double latestY = 0;

        public CruiseState newY(double y){
            yVariant = MathUtils.clamp(y - latestY + yVariant, -20, 20);
            return this;
        }

        public double cruiseRatio(){
            return Math.sqrt((1.001 - 0.5 * yVariant / 20));
        }
    }

}
