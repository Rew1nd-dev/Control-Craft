package com.verr1.controlcraft.unstable.ai.core.nodes;

import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;

public abstract class Condition implements Node {
    @Override
    public Status execute(Blackboard blackboard) {
        return check(blackboard) ? Status.SUCCESS : Status.FAILURE;
    }

    protected abstract boolean check(Blackboard blackboard);
}
