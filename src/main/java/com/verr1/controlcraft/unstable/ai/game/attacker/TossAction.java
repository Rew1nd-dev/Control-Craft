package com.verr1.controlcraft.unstable.ai.game.attacker;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.unstable.ai.api.IAttackerContext;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;
import com.verr1.controlcraft.unstable.ai.core.nodes.Action;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.blocks.attacker.AiAttackerBlockEntity;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.Optional;

public class TossAction extends Action {
    @Override
    protected Status perform(Blackboard blackboard) {
        IAttackerContext context = blackboard.get(SharedAIKeys.ATTACKER_CONTEXT);
        if(context == null)return Status.FAILURE;
        Vector3dc dir = context.getGroundTarget().sub(context.getPosition(), new Vector3d());
        double angle = context.getHeading().angle(dir);
        context.fireAt(angle < context.shootTolerance() ? dir : context.getHeading());
        // ControlCraft.LOGGER.info("tossed");
        return Status.SUCCESS;
    }
}
