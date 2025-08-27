package com.verr1.controlcraft.unstable.ai.game.cruiser.conditions;

import com.verr1.controlcraft.unstable.ai.api.IAirContext;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.nodes.Condition;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.ai.game.cruiser.AirBaseAwareness;
import org.joml.Vector3d;
import org.joml.Vector3dc;

public class FighterPullEnterCondition extends Condition {
    @Override
    protected boolean check(Blackboard blackboard) {
        AirBaseAwareness awareness = blackboard.get(SharedAIKeys.AWARENESS);
        IAirContext context = blackboard.get(SharedAIKeys.AIR_COMMON);
        if(awareness == null)return false;
        double height = awareness.currentHeight();
        double obstacle = awareness.currentObstacleDistance();
        Vector3dc head = context.getHeading().normalize(new Vector3d());

        double collideT0 = obstacle / context.cruiseVelocity();


        return collideT0 < 1 || height < context.extremeRadius();
    }
}
