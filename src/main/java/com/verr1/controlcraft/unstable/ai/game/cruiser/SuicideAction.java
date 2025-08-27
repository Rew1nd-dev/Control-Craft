package com.verr1.controlcraft.unstable.ai.game.cruiser;

import com.verr1.controlcraft.unstable.ai.api.IAirContext;
import com.verr1.controlcraft.unstable.ai.api.IFighterJetContext;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;
import com.verr1.controlcraft.unstable.ai.core.nodes.Action;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;

public class SuicideAction extends Action {
    @Override
    protected Status perform(Blackboard blackboard) {
        IAirContext context = blackboard.get(SharedAIKeys.AIR_COMMON);
        AirBaseAwareness awareness = blackboard.get(SharedAIKeys.AWARENESS);
        if (context == null || awareness == null)return Status.RUNNING;
        if(awareness.shouldDiscard()){
            context.kill();
        }
        return Status.RUNNING;
    }
}
