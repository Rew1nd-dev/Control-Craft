package com.verr1.controlcraft.unstable.ai.game.cruiser;

import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.nodes.Condition;
import com.verr1.controlcraft.unstable.ai.game.cruiser.v1.Situation;
import com.verr1.controlcraft.unstable.blocks.cruiser.CruiserBlockEntity;

public class DecisionCondition extends Condition {
    @Override
    protected boolean check(Blackboard blackboard) {
        return false;// blackboard.computeIfAbsent(CruiserBlockEntity.AWARENESS, Situation::new).peekDecision();
    }
}
