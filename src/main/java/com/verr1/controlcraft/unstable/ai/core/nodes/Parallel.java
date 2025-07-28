package com.verr1.controlcraft.unstable.ai.core.nodes;

import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;

public class Parallel extends CompositeNode {

    private final ParallelPolicy policy;
    private Status[] childStatuses; // 记录每个子节点的状态

    public Parallel(ParallelPolicy policy) {
        this.policy = policy;
    }

    @Override
    public Status execute(Blackboard board) {
        // 第一次执行时初始化状态数组
        if (childStatuses == null || childStatuses.length != children.size()) {
            childStatuses = new Status[children.size()];
            for (int i = 0; i < children.size(); i++) {
                childStatuses[i] = Status.RUNNING;
            }
        }

        // 执行所有仍在运行中的子节点
        boolean anyRunning = false;
        int successCount = 0;
        int failureCount = 0;

        for (int i = 0; i < children.size(); i++) {
            // 只tick之前还在运行的节点
            if (childStatuses[i] == Status.RUNNING) {
                childStatuses[i] = children.get(i).execute(board);
            }

            if (childStatuses[i] == Status.RUNNING) {
                anyRunning = true;
            } else if (childStatuses[i] == Status.SUCCESS) {
                successCount++;
            } else if (childStatuses[i] == Status.FAILURE) {
                failureCount++;
            }
        }

        // 如果还有节点在运行，返回RUNNING
        if (anyRunning) {
            return Status.RUNNING;
        }

        // 根据策略确定并行节点状态
        return switch (policy) {
            case SUCCEED_ON_ALL -> (successCount == children.size()) ?
                    Status.SUCCESS : Status.FAILURE;
            case SUCCEED_ON_ONE -> (successCount > 0) ?
                    Status.SUCCESS : Status.FAILURE;
            case SUCCEED_ON_MAJORITY -> (successCount > failureCount) ?
                    Status.SUCCESS : Status.FAILURE;
        };
    }





    @Override
    public void reset() {
        // 重置所有子节点状态
        for (Node child : children) {
            child.reset();
        }
        childStatuses = null; // 重置状态记录
    }
}
