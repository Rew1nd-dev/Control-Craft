package com.verr1.controlcraft.unstable.ai.core.nodes;

import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;

public class Inverter extends Decorator {
    public Inverter(Node child) {
        super(child);
    }

    @Override
    public Status execute(Blackboard blackboard) {
        Status status = child.execute(blackboard);
        return switch (status){
            case SUCCESS -> Status.FAILURE;
            case FAILURE -> Status.SUCCESS;
            case RUNNING -> Status.RUNNING;
        };
    }
}
