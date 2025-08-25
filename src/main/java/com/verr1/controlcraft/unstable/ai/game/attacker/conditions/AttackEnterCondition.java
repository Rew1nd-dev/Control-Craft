package com.verr1.controlcraft.unstable.ai.game.attacker.conditions;

import com.verr1.controlcraft.unstable.ai.api.IAttackerContext;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.nodes.Condition;
import com.verr1.controlcraft.unstable.blocks.attacker.AiAttackerBlockEntity;

public class AttackEnterCondition extends Condition {
    @Override
    protected boolean check(Blackboard blackboard) {
        IAttackerContext context = blackboard.get(AiAttackerBlockEntity.CONTEXT);
        if (context == null)return false;
        return !context.noGroundTarget() && !context.hasAirThreat();
    }
}
