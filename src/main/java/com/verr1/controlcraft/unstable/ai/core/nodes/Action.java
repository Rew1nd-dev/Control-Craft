package com.verr1.controlcraft.unstable.ai.core.nodes;

import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;

public abstract class Action implements Node {


    @Override
    public Status execute(Blackboard blackboard) {
        return perform(blackboard);
    }

    protected abstract Status perform(Blackboard blackboard);


}