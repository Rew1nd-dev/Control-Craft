package com.verr1.controlcraft.unstable.ai.core.nodes;

import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;

public class Interrupt extends Decorator {
    private final Node condition;
    private boolean wasConditionMet;
    private boolean hasInterrupted;

    public Interrupt(Node condition, Node child) {
        super(child);
        this.condition = condition;
        this.wasConditionMet = false;
    }

    @Override
    public Status execute(Blackboard board) {
        // 检查条件是否发生变化
        boolean isConditionMet = condition.execute(board) == Status.SUCCESS;

        // 如果条件状态发生变化（从满足变为不满足）
        if (wasConditionMet && !isConditionMet) {
            // 中断子节点
            if (child instanceof Interruptible) {
                ((Interruptible) child).interrupt();
            }
            child.reset();
            hasInterrupted = true;
            wasConditionMet = false;
            return Status.FAILURE;
        }

        wasConditionMet = isConditionMet;

        // 如果已经中断过，直接返回失败
        if (hasInterrupted) {
            return Status.FAILURE;
        }

        if(!isConditionMet){
            return Status.FAILURE;
        }
        Status status = child.execute(board);

        // 如果子节点完成，重置状态
        if (status != Status.RUNNING) {
            reset();
        }

        return status;
    }

    @Override
    public void reset() {
        hasInterrupted = false;
        wasConditionMet = false;
        child.reset();
    }
}
