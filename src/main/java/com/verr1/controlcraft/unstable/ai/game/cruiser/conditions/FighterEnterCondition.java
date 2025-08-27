package com.verr1.controlcraft.unstable.ai.game.cruiser.conditions;

import com.verr1.controlcraft.unstable.ai.api.IAttackerContext;
import com.verr1.controlcraft.unstable.ai.api.IFighterJetContext;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.nodes.Condition;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;

public class FighterEnterCondition extends Condition {
    @Override
    protected boolean check(Blackboard blackboard) {
        IFighterJetContext context = blackboard.get(SharedAIKeys.FIGHTER_CONTEXT);
        if (context == null)return false;
        return !context.noTarget(); // && !context.hasAirThreat();
    }
}
