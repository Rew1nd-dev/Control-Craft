package com.verr1.controlcraft.unstable.management;

public class ImmediateRoutine extends CoroutineBase{
    final Runnable task;

    public ImmediateRoutine(Runnable task) {
        this.task = task;
    }


    @Override
    protected RoutineStatus run(int steps) {
        task.run();
        close();
        return RoutineStatus.FINISHED_ALL;
    }

    @Override
    protected void init() {

    }

    @Override
    protected boolean canResume() {
        return !closed;
    }

    @Override
    public int suggestedBatch() {
        return 1;
    }
}
