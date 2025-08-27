package com.verr1.controlcraft.unstable.ai.game.attacker.conditions;

import com.verr1.controlcraft.unstable.ai.api.IAirContext;
import com.verr1.controlcraft.unstable.ai.api.IAttackerContext;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.nodes.Condition;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.ai.game.cruiser.AirBaseAwareness;

public class AttackerPullEnterCondition extends Condition {
    @Override
    protected boolean check(Blackboard blackboard) {
        AirBaseAwareness awareness = blackboard.get(SharedAIKeys.AWARENESS);
        IAttackerContext context = blackboard.get(SharedAIKeys.ATTACKER_CONTEXT);
        if(awareness == null)return false;
        double height = awareness.currentHeight();
        double obstacle = awareness.currentObstacleDistance();
        double collideT0 = obstacle / context.cruiseVelocity();

        return height < context.cruiseRadius() || collideT0 < 1;
    }
}
