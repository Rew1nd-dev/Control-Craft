package com.verr1.controlcraft.unstable.ai.game.attacker;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.unstable.ai.api.IAttackerContext;
import com.verr1.controlcraft.unstable.ai.core.Address;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;
import com.verr1.controlcraft.unstable.ai.core.nodes.Action;
import com.verr1.controlcraft.unstable.ai.core.nodes.Interruptible;
import com.verr1.controlcraft.unstable.blocks.attacker.AiAttackerBlockEntity;
import com.verr1.controlcraft.utils.MathUtils;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.Random;

public class CirclingAction extends Action implements Interruptible {

    public static final Random random = new Random();
    public static final Address<Double> LATEST_Y = new Address<>("latest_target", Double.class);
    public static final Address<Vector3dc> LOOKAHEAD = new Address<>("prev_lookAhead", Vector3dc.class);

    @Override
    protected Status perform(Blackboard blackboard) {
        ControlCraft.LOGGER.info("try circling");
        IAttackerContext context = blackboard.get(AiAttackerBlockEntity.CONTEXT);
        // AirBaseAwareness awareness = blackboard.get(SharedAIKeys.AWARENESS);
        if(context == null)return Status.FAILURE;
        Vector3dc latestAnchor = blackboard.get(AnchorAction.LATEST);

        double latestY = blackboard.computeIfAbsent(LATEST_Y, () -> -256.0);
        double y0 = context.height() + context.extremeRadius() * 1.5;
        double cr = context.extremeRadius() * 3;

        double yt = MathUtils.clamp(latestY, y0, y0 + context.extremeRadius());
        blackboard.set(LATEST_Y, yt);

        Vector3dc prevLookAhead = blackboard.computeIfAbsent(LOOKAHEAD, Vector3d::new);
        Vector3dc lookaheadPoint = computeLookaheadPoint(
                context.getPosition(),
                context.getVelocity(),
                latestAnchor,
                yt,
                cr,
                Math.PI / 4, // 最大偏置角度
                context.extremeRadius() * 4, // 区域半径
                0.1, // 平滑因子
                0.5, // 噪声幅度
                prevLookAhead
        );

        blackboard.set(LOOKAHEAD, lookaheadPoint);
        Vector3dc dir = lookaheadPoint.sub(context.getPosition(), new Vector3d());
        context.controller().overrideTarget(dir);

        return Status.RUNNING;

    }

    @Override
    public void interrupt(Blackboard board) {
        IAttackerContext context = board.get(AiAttackerBlockEntity.CONTEXT);
        if (context == null)return;
        board.remove(LOOKAHEAD);
    }

    public static Vector3d computeLookaheadPoint(
            Vector3dc currentPos,
            Vector3dc currentVel,
            Vector3dc baseXZ,
            double cruiseY,
            double lookaheadDist,
            double maxBiasAngle,
            double regionRadius,
            double smoothFactor,
            double noiseAmp,
            Vector3dc prevLookahead
    ) {

        // 步骤1: 获取单位方向 (处理速度为0的情况)
        double velLength = currentVel.length();
        Vector3d unitDir = new Vector3d(currentVel);
        if (velLength < 1e-6) {
            unitDir = new Vector3d(baseXZ).sub(currentPos).normalize(); // 默认朝向基准
        } else {
            unitDir.div(velLength);
        }

        // 步骤2: 添加随机偏置角度
        double biasAngle = random.nextDouble() * maxBiasAngle * (random.nextBoolean() ? 1 : -1);
        // 旋转unitDir (使用JOML旋转)
        Vector3d biasedDir = new Vector3d(unitDir).rotateY(biasAngle);

        // 计算基础前方点
        Vector3d forwardPoint = new Vector3d(currentPos).add(biasedDir.mul(lookaheadDist));

        // 步骤3: 约束到区域
        Vector3d offsetFromBase = new Vector3d(forwardPoint).sub(baseXZ);
        offsetFromBase.y = 0; // 忽略y
        double distToBase = offsetFromBase.length();
        if (distToBase > regionRadius) {
            offsetFromBase.normalize().mul(regionRadius);
            forwardPoint = new Vector3d(baseXZ).add(offsetFromBase);
        }

        // 步骤4: 添加随机噪声
        forwardPoint.add(random.nextDouble() * noiseAmp - noiseAmp / 2, 0, random.nextDouble() * noiseAmp - noiseAmp / 2);


        Vector3d newLookAhead = new Vector3d(prevLookahead);
        // 步骤5: 平滑更新 (lerp)
        if (newLookAhead.lengthSquared() == 0) {
            newLookAhead.set(forwardPoint); // 首次初始化
        }
        newLookAhead.lerp(forwardPoint, smoothFactor); // 更新prev

        // 设置y高度
        newLookAhead.y = cruiseY;

        return newLookAhead;
    }



}
