package com.verr1.controlcraft.unstable.ai.game.cruiser.conditions;

import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.nodes.Condition;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.ai.game.cruiser.AirBaseAwareness;

public class PullEnterCondition extends Condition {
    @Override
    protected boolean check(Blackboard blackboard) {
        AirBaseAwareness awearness = blackboard.get(SharedAIKeys.AWARENESS);
        if(awearness == null)return false;
        return awearness.shouldPullUp();
    }
}
