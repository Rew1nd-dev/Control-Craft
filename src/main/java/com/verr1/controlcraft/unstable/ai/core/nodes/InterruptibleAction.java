package com.verr1.controlcraft.unstable.ai.core.nodes;

import com.verr1.controlcraft.unstable.ai.core.Blackboard;

public abstract class InterruptibleAction extends Action implements Interruptible {
    protected boolean interrupted = false;


    @Override
    public void interrupt(Blackboard board) {
        interrupted = true;
        onInterrupted(board);
    }

    protected abstract void onInterrupted(Blackboard board);

    @Override
    public void reset() {
        interrupted = false;
        super.reset();
    }
}
