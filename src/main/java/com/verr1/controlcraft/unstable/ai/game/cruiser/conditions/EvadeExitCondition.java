package com.verr1.controlcraft.unstable.ai.game.cruiser.conditions;

import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.nodes.Condition;
import com.verr1.controlcraft.unstable.ai.game.cruiser.AirAwareness;
import com.verr1.controlcraft.unstable.blocks.cruiser.CruiserBlockEntity;

public class EvadeExitCondition extends Condition {
    @Override
    protected boolean check(Blackboard blackboard) {
        AirAwareness awearness = blackboard.get(CruiserBlockEntity.AWARENESS);
        if(awearness == null)return false;

        return !awearness.isInLossCone()
                || awearness.distance() > 3 * awearness.safeDistance();


    }
}
