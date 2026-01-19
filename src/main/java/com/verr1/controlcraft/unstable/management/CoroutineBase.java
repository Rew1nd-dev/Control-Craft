package com.verr1.controlcraft.unstable.management;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.utils.DebugUtils;

public abstract class CoroutineBase implements IRoutine{
    private int count = 0;
    protected boolean closed = false;

    protected abstract RoutineStatus run(int steps);

    protected abstract void init();

    protected abstract boolean canResume();

    @Override
    public RoutineStatus resume(int steps) {
        if(closed){
            ControlCraft.LOGGER.error("Trying to resume a closed Routine!!, class: {}",
                this.getClass().getSimpleName()
            );
            DebugUtils.printStackTrace();
        }
        if(!canResume())return RoutineStatus.INVALID;
        if(count == 0){
            init();
        }
        count++;
        var res = run(steps);
        if(res == RoutineStatus.FINISHED_ALL){
            closed = true;
        }
        return res;
    }

    @Override
    public int count() {
        return count;
    }

    protected void close(){
        closed = true;
    }

    @Override
    public boolean closed() {
        return closed;
    }

    public int suggestedBatch(){
        return 1;
    }

    @Override
    public void abort() {
        close();
    }

    public void force(){
        while (!closed()){
            resume(suggestedBatch());
        }
    }

}
