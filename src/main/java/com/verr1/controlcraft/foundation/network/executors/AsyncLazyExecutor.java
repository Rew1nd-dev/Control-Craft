package com.verr1.controlcraft.foundation.network.executors;

import com.verr1.controlcraft.utils.TimeCache;

public class AsyncLazyExecutor {

    private final Runnable task;
    private boolean shouldRun = false;
    private long cooldownMillis;
    private long lastExecutionTime = 0;

    public AsyncLazyExecutor(Runnable task) {
        this.task = task;
        this.cooldownMillis = 10;
    }

    public void setCooldownMillis(long cooldownMillis) {
        this.cooldownMillis = cooldownMillis;
    }

    public void executeAsync() {
        long currentTime = TimeCache.time();
        if (currentTime - lastExecutionTime >= cooldownMillis) {
            shouldRun = true;
            lastExecutionTime = currentTime;
        }
    }

    public void execute(){
        if(shouldRun){
            task.run();
            shouldRun = false;
        }
    }


}
