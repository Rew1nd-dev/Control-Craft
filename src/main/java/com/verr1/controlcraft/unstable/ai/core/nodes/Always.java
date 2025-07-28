package com.verr1.controlcraft.unstable.ai.core.nodes;

import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;

public class Always extends Decorator{
    public Always(Node child) {
        super(child);
    }

    @Override
    public Status execute(Blackboard board) {
        child.execute(board);
        return Status.RUNNING;
    }
}
