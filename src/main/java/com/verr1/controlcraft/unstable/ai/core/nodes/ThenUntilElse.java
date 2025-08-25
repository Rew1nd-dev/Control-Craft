package com.verr1.controlcraft.unstable.ai.core.nodes;

import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;

public class ThenUntilElse implements Node, Interruptible{

    private final Node thenNode;
    private final Node elseNode;

    private final Node enter;
    private final Node exit;

    private boolean isEntered = false;

    public ThenUntilElse(
            Node enter,
            Node thenNode,
            Node exit,
            Node elseNode
    ) {
        this.thenNode = thenNode;
        this.elseNode = elseNode;
        this.enter = enter;
        this.exit = exit;
    }


    @Override
    public Status execute(Blackboard board) {

        if (!isEntered) {
            if (enter.execute(board) == Status.SUCCESS) {
                isEntered = true;
                interrupt(elseNode, board);
            }
        }else{
            if (exit.execute(board) == Status.SUCCESS) {
                isEntered = false;
                interrupt(thenNode, board);
            }
        }

        if (isEntered) {
            return executeAndReset(thenNode, board);
        } else {
            return executeAndReset(elseNode, board);
        }
    }

    private void interrupt(Node node, Blackboard blackboard) {
        if (node instanceof Interruptible interruptible) {
            interruptible.interrupt(blackboard);
        }
        node.reset();
    }

    private Status executeAndReset(Node node, Blackboard blackboard){
        Status status = node.execute(blackboard);
        if (status == Status.RUNNING) {
            return Status.RUNNING;
        } else {
            node.reset();
            return status;
        }
    }

    @Override
    public void reset() {
        isEntered = false;
        thenNode.reset();
        elseNode.reset();
    }

    @Override
    public void interrupt(Blackboard board) {
        if (isEntered) {
            interrupt(thenNode, board);
        } else {
            interrupt(elseNode, board);
        }
    }
}
