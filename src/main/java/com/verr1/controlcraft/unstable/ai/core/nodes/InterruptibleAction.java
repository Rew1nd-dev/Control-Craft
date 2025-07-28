package com.verr1.controlcraft.unstable.ai.core.nodes;

public abstract class InterruptibleAction extends Action implements Interruptible {
    protected boolean interrupted = false;


    @Override
    public void interrupt() {
        interrupted = true;
        onInterrupted();
    }

    protected abstract void onInterrupted();

    @Override
    public void reset() {
        interrupted = false;
        super.reset();
    }
}
