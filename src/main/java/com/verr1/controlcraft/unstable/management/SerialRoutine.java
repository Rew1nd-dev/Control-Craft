package com.verr1.controlcraft.unstable.management;

import org.jetbrains.annotations.Nullable;

import java.util.ArrayDeque;
import java.util.List;
import java.util.Queue;

public class SerialRoutine extends CoroutineBase{


    Queue<CoroutineBase> queue;

    public SerialRoutine(List<CoroutineBase> allRoutines){
        queue = new ArrayDeque<>(allRoutines);
    }



    @Override
    protected RoutineStatus run(int steps) {
        CoroutineBase current = current();
        if(current == null){
            close();
            return RoutineStatus.FINISHED_ALL;
        }

        var res = current.resume(steps);
        if(res != RoutineStatus.FINISHED_STEP){
            queue.poll();
            if(queue.isEmpty()){
                close();
                return RoutineStatus.FINISHED_ALL;
            }
        }

        return RoutineStatus.FINISHED_STEP;
    }

    @Override
    protected void init() {

    }

    protected @Nullable CoroutineBase current(){
        return queue.peek();
    }

    @Override
    protected boolean canResume() {
        CoroutineBase current = current();
        return current == null || current.canResume();
    }

    @Override
    public int suggestedBatch() {
        CoroutineBase current = current();
        return current == null ? super.suggestedBatch() : current.suggestedBatch();
    }
}
