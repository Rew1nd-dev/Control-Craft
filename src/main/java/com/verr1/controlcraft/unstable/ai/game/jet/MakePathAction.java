package com.verr1.controlcraft.unstable.ai.game.jet;

import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;
import com.verr1.controlcraft.unstable.ai.core.nodes.Action;
import com.verr1.controlcraft.unstable.blocks.jet.AiJetBlockEntity;
import com.verr1.controlcraft.unstable.pathing.dubins.DubinsCalculatorV2;
import com.verr1.controlcraft.unstable.pathing.path.IPath;
import com.verr1.controlcraft.unstable.valkyrienskies.context.CruiserControllerV4;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import static com.verr1.controlcraft.unstable.ai.game.jet.PathAlongAction.LATEST_ACCUMULATED;
import static com.verr1.controlcraft.unstable.blocks.jet.AiJetBlockEntity.CURRENT_CRUISE;


import static com.verr1.controlcraft.unstable.valkyrienskies.controls.AIControlUtils.*;
import static com.verr1.controlcraft.utils.MathUtils.*;

public class MakePathAction extends Action {
    @Override
    protected Status perform(Blackboard blackboard) {
        AiJetBlockEntity context = blackboard.get(AiJetBlockEntity.CONTEXT);
        if (context == null)return Status.RUNNING;
        CruiserControllerV4 controller = context.controller();
        Vector3dc pos = context.getPosition();
        Vector3dc vel = context.getHeading();
        Vector3dc t_pos = context.debug_getTarget();
        Vector3dc t_vel = context.debug_getTargetVelocity();

        if(t_pos == null || t_vel == null)return Status.SUCCESS;
        System.out.println("making new path");
        IPath path = DubinsCalculatorV2.dubins(
                pos, safeNormalize(vel, new Vector3d(0, 1, 0)),
                t_pos, safeNormalize(t_vel, new Vector3d(0, 1, 0)),
                50
        );

        if(path == null)return Status.FAILURE;
        blackboard.set(LATEST_ACCUMULATED, 0.0);
        blackboard.set(CURRENT_CRUISE, path);

        return Status.SUCCESS;
    }
}
