package com.verr1.controlcraft.foundation.network.executors;

import com.verr1.controlcraft.utils.TimeCache;

public class LazyExecutor {

    private final Runnable task;
    private final long cooldownMillis;
    private long lastExecutionTime = 0;

    public LazyExecutor(Runnable task, long cooldownMillis) {
        this.task = task;
        this.cooldownMillis = cooldownMillis;
    }

    public void execute() {
        long currentTime = TimeCache.time();
        if (currentTime - lastExecutionTime >= cooldownMillis) {
            task.run();
            lastExecutionTime = currentTime;
        }
    }

}
