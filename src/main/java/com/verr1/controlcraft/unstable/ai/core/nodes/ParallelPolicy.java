package com.verr1.controlcraft.unstable.ai.core.nodes;

public enum ParallelPolicy {
    SUCCEED_ON_ALL,    // 所有子节点成功才算成功
    SUCCEED_ON_ONE,    // 一个子节点成功就算成功
    SUCCEED_ON_MAJORITY // 多数子节点成功才算成功
}
