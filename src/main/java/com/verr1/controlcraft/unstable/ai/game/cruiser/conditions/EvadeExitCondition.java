package com.verr1.controlcraft.unstable.ai.game.cruiser.conditions;

import com.verr1.controlcraft.unstable.ai.api.IAirContext;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.nodes.Condition;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.ai.game.cruiser.AirBaseAwareness;

public class EvadeExitCondition extends Condition {
    @Override
    protected boolean check(Blackboard blackboard) {
        AirBaseAwareness awareness = blackboard.get(SharedAIKeys.AWARENESS);
        IAirContext context = blackboard.get(SharedAIKeys.AIR_COMMON);
        if(awareness == null || context == null)return false;

        return !awareness.isInLossCone(2.5 * context.cruiseVelocity())
                || awareness.distance() > 3 * awareness.safeDistance();


    }
}
