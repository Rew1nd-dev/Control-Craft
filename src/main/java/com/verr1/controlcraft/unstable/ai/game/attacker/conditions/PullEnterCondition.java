package com.verr1.controlcraft.unstable.ai.game.attacker.conditions;

import com.verr1.controlcraft.unstable.ai.api.IAttackerContext;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.nodes.Condition;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.ai.game.cruiser.AirBaseAwareness;
import com.verr1.controlcraft.unstable.blocks.attacker.AiAttackerBlockEntity;

public class PullEnterCondition extends Condition {
    @Override
    protected boolean check(Blackboard blackboard) {
        AirBaseAwareness awareness = blackboard.get(SharedAIKeys.AWARENESS);
        if (awareness == null)return false;
        return awareness.tooLow() || awareness.tooClose();
    }
}
