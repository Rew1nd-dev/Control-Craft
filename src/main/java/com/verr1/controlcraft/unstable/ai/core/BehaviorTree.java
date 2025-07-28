package com.verr1.controlcraft.unstable.ai.core;

import com.verr1.controlcraft.unstable.ai.core.nodes.Node;

public class BehaviorTree {
    private Node root;

    public BehaviorTree(Node root) {
        this.root = root;
    }

    public void update(Blackboard blackboard) {
        root.execute(blackboard);
    }
}
