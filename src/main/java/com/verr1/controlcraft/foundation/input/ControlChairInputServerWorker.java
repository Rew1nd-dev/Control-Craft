package com.verr1.controlcraft.foundation.input;

import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;

public final class ControlChairInputServerWorker {
    private static final Object LOCK = new Object();

    private static ExecutorService executor = null;

    private ControlChairInputServerWorker() {}

    public static void start() {
        synchronized (LOCK) {
            if (executor != null && !executor.isShutdown()) {
                return;
            }

            ThreadFactory factory = runnable -> {
                Thread thread = new Thread(runnable, "ControlCraft-ControlChairInput");
                thread.setDaemon(true);
                return thread;
            };
            executor = Executors.newSingleThreadExecutor(factory);
        }
    }

    public static void stop() {
        synchronized (LOCK) {
            if (executor != null) {
                executor.shutdownNow();
                executor = null;
            }
        }
    }

    public static void submit(UUID playerId, ControlChairInputDelta delta) {
        ExecutorService current;
        synchronized (LOCK) {
            current = executor;
        }

        if (current == null || current.isShutdown()) {
            return;
        }

        current.execute(() -> ControlChairInputManager.accept(playerId, delta));
    }
}
