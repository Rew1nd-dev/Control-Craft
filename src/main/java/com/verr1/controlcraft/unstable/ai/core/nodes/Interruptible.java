package com.verr1.controlcraft.unstable.ai.core.nodes;

import com.verr1.controlcraft.unstable.ai.core.Blackboard;

public interface Interruptible {
    void interrupt(Blackboard board);
}
