package com.verr1.controlcraft.unstable.ai.game.attacker;

import com.verr1.controlcraft.unstable.ai.api.IAttackerContext;
import com.verr1.controlcraft.unstable.ai.core.Address;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;
import com.verr1.controlcraft.unstable.ai.core.nodes.Action;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.pathing.dubins.DubinsCalculatorV2;
import com.verr1.controlcraft.unstable.pathing.path.IPath;
import com.verr1.controlcraft.unstable.pathing.path.LinePath;
import com.verr1.controlcraft.utils.MathUtils;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import static com.verr1.controlcraft.unstable.ai.game.attacker.PathAlongAction.LATEST_ACCUMULATED;
import static com.verr1.controlcraft.unstable.blocks.attacker.AiAttackerBlockEntity.CURRENT_CRUISE;


import static com.verr1.controlcraft.utils.MathUtils.*;

public class MakeAdjustPathAction extends Action {

    public static final Address<Double> ENTER_YAW = new Address<>("enter_yaw", Double.class);

    public static final Address<Double> ENTER_PITCH = new Address<>("enter_pitch", Double.class);

    @Override
    protected Status perform(Blackboard blackboard) {
        IAttackerContext context = blackboard.get(SharedAIKeys.ATTACKER_CONTEXT);
        if (context == null)return Status.RUNNING;
        Vector3dc pos = context.getPosition();
        Vector3dc vel = context.getHeading();
        Vector3dc t_pos = context.getGroundTarget();

        if(t_pos == null)return Status.SUCCESS;

        // System.out.println("making new path");

        double enterYaw = Math.random() * 2 * Math.PI;
        double enterPitch = Math.toRadians(MathUtils.lerp(Math.random(), 30, 60));

        blackboard.set(ENTER_YAW, enterYaw);
        blackboard.set(ENTER_PITCH, enterPitch);

        double safeHeight = context.cruiseRadius() * 1;
        double strikeDistance = 2 * context.cruiseRadius();
        double endDistance = safeHeight / Math.sin(enterPitch);

        Vector3dc strikeDirection = new Vector3d(Math.cos(enterYaw), Math.tan(enterPitch), Math.sin(enterYaw)).normalize();
        Vector3dc strikeEnd = t_pos.fma(endDistance, strikeDirection, new Vector3d());
        Vector3dc strikeStart = strikeEnd.fma(strikeDistance, strikeDirection, new Vector3d());

        IPath strike = new LinePath(
                strikeStart,
                strikeEnd
        );

        Vector3dc strikeDirReal = strikeDirection.negate(new Vector3d());
        Vector3dc strikeStartDirection = new Vector3d(strikeDirReal.x(), 0, strikeDirReal.z());
        Vector3dc circleStart = new Vector3d(
                2 * t_pos.x() - strikeStart.x(),
                strikeStart.y(),
                2 * t_pos.z() - strikeStart.z()
        );
        Vector3dc circleStartDirection = strikeStartDirection.rotateY(Math.toRadians(90), new Vector3d());


        IPath adjust0 = DubinsCalculatorV2.dubinsMatchEnd(
                pos, safeNormalize(vel, new Vector3d(0, 1, 0)),
                circleStart, circleStartDirection,
                context.cruiseRadius()
        );

        IPath adjust1 = DubinsCalculatorV2.dubinsMatchEnd(
                circleStart, circleStartDirection,
                strikeStart, strikeStartDirection,
                context.cruiseRadius()
        );

        if(adjust0 == null || adjust1 == null)return Status.FAILURE;

        IPath total = IPath.concat(adjust0, adjust1, strike);


        blackboard.set(LATEST_ACCUMULATED, 0.0);
        blackboard.set(CURRENT_CRUISE, total);

        return Status.SUCCESS;
    }
}
