package com.verr1.controlcraft.unstable.ai.core.nodes;

import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;

public class Selector extends CompositeNode implements Interruptible{



    @Override
    public Status execute(Blackboard blackboard) {
        while(currentChildIndex < children.size()){

            Status status = children.get(currentChildIndex).execute(blackboard);

            switch (status){
                case SUCCESS:
                    reset();
                    return Status.FAILURE;
                case FAILURE:
                    break;
                case RUNNING:
                    lastStatus = Status.RUNNING;
                    return Status.RUNNING;
            }

            currentChildIndex++;
        }
        reset();
        return Status.FAILURE;
    }


    @Override
    public void interrupt(Blackboard board) {
        if(lastStatus == Status.RUNNING && currentChildIndex < children.size()){
            Node current = children.get(currentChildIndex);
            if(current instanceof Interruptible interruptible){
                interruptible.interrupt(board);
            }
        }
        reset();
    }
}
