package com.verr1.controlcraft.unstable.ai.core.nodes;

public abstract class Decorator implements Node {
    protected final Node child;

    public Decorator(Node child) {
        this.child = child;
    }

    @Override
    public void reset() {
        child.reset();
    }
}
