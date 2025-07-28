package com.verr1.controlcraft.unstable.ai.core.nodes;

import com.verr1.controlcraft.unstable.ai.core.Status;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public abstract class CompositeNode implements Node {
    protected final List<Node> children = new ArrayList<>();
    protected Status lastStatus = Status.FAILURE;
    protected int currentChildIndex = 0; // 当前执行的子节点索引

    public CompositeNode addChild(Node child) {
        children.add(child);
        return this;
    }

    public CompositeNode addChild(Node... node){
        children.addAll(Arrays.asList(node));
        return this;
    }


    @Override
    public void reset() {
        currentChildIndex = 0;
        lastStatus = Status.FAILURE;
        children.forEach(Node::reset);
    }


}


