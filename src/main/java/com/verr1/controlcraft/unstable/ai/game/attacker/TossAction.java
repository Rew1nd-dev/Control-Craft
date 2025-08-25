package com.verr1.controlcraft.unstable.ai.game.attacker;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.unstable.ai.api.IAttackerContext;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;
import com.verr1.controlcraft.unstable.ai.core.nodes.Action;
import com.verr1.controlcraft.unstable.blocks.attacker.AiAttackerBlockEntity;

public class TossAction extends Action {
    @Override
    protected Status perform(Blackboard blackboard) {
        IAttackerContext context = blackboard.get(AiAttackerBlockEntity.CONTEXT);
        if(context == null)return Status.FAILURE;
        context.fireAt(context.getHeading());
        ControlCraft.LOGGER.info("tossed");
        return Status.SUCCESS;
    }
}
