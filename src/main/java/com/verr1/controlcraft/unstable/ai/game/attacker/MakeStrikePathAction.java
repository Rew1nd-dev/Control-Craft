package com.verr1.controlcraft.unstable.ai.game.attacker;

import com.verr1.controlcraft.unstable.ai.api.IAttackerContext;
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

import static com.verr1.controlcraft.unstable.ai.game.attacker.MakeAdjustPathAction.ENTER_YAW;
import static com.verr1.controlcraft.unstable.ai.game.attacker.PathAlongAction.LATEST_ACCUMULATED;
import static com.verr1.controlcraft.unstable.blocks.attacker.AiAttackerBlockEntity.CURRENT_CRUISE;
import static com.verr1.controlcraft.utils.MathUtils.safeNormalize;

public class MakeStrikePathAction extends Action {
    @Override
    protected Status perform(Blackboard blackboard) {
        IAttackerContext context = blackboard.get(SharedAIKeys.ATTACKER_CONTEXT);
        if (context == null)return Status.RUNNING;
        Vector3dc pos = context.getPosition();
        Vector3dc vel = context.getHeading();
        Vector3dc t_pos = context.getGroundTarget();

        if(t_pos == null)return Status.SUCCESS;

        // System.out.println("making new path");

        double enterYaw = blackboard.computeIfAbsent(ENTER_YAW, () -> Math.random() * 2 * Math.PI);
        double enterPitch = blackboard.computeIfAbsent(ENTER_YAW, () -> Math.toRadians(MathUtils.lerp(Math.random(), 30, 60)));

        Vector3dc strikeDirection = new Vector3d(Math.cos(enterYaw), Math.tan(enterPitch), Math.sin(enterYaw)).normalize();


        IPath adjust0 = DubinsCalculatorV2.dubinsMatchEnd(
                pos, safeNormalize(vel, new Vector3d(0, 1, 0)),
                t_pos, strikeDirection.negate(new Vector3d()),
                context.cruiseRadius()
        );


        if(adjust0 == null)return Status.FAILURE;


        blackboard.set(LATEST_ACCUMULATED, 0.0);
        blackboard.set(CURRENT_CRUISE, adjust0);

        return Status.SUCCESS;
    }
}
