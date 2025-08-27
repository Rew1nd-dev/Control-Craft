package com.verr1.controlcraft.unstable.ai.game.attacker.conditions;

import com.verr1.controlcraft.unstable.ai.api.IAttackerContext;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.nodes.Condition;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.ai.game.cruiser.AirBaseAwareness;

public class AttackerEvadeEnterCondition extends Condition {
    @Override
    protected boolean check(Blackboard blackboard) {
        IAttackerContext context = blackboard.get(SharedAIKeys.ATTACKER_CONTEXT);
        AirBaseAwareness awareness = blackboard.get(SharedAIKeys.AWARENESS);
        if(awareness == null)return false;
        return (awareness.isInLossCone(2 * context.cruiseRadius()) || awareness.closingRate() < 2 || context.fireCooldown() > 100); // || awareness.attackScore() < 5
    }
}
