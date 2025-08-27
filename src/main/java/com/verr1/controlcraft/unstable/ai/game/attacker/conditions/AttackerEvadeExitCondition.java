package com.verr1.controlcraft.unstable.ai.game.attacker.conditions;

import com.verr1.controlcraft.unstable.ai.api.IAttackerContext;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.nodes.Condition;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.ai.game.cruiser.AirBaseAwareness;

public class AttackerEvadeExitCondition extends Condition {
    @Override
    protected boolean check(Blackboard blackboard) {
        AirBaseAwareness awareness = blackboard.get(SharedAIKeys.AWARENESS);
        IAttackerContext context = blackboard.get(SharedAIKeys.ATTACKER_CONTEXT);
        if(awareness == null)return false;

        return //(!awareness.isInLossCone(8 * context.cruiseRadius()) ||
                awareness.distance() > 3.2 * context.cruiseRadius() && context.fireCooldown() < 100;


    }
}
