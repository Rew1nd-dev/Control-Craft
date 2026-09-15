package com.verr1.controlcraft.foundation.cimulink.core.components.lua;

import com.sun.management.ThreadMXBean;
import org.luaj.vm2.LuaError;
import org.luaj.vm2.Varargs;
import org.luaj.vm2.lib.DebugLib;

import java.lang.management.ManagementFactory;

public class WatcherLib extends DebugLib {

    private volatile boolean interrupted = false;
    private final ThreadMXBean threadMXBean;
    private int maxInstructions = Integer.MAX_VALUE;
    private int instructionCount = 0;
    private long maxNanos = Long.MAX_VALUE;
    private long startTime = 0L;
    private long startAllocatedBytes = -1L;
    private long maxAllocatedBytes = 10 * 1024 * 1024L;

    public WatcherLib() {
        this.threadMXBean = (ThreadMXBean) ManagementFactory.getThreadMXBean();
        if (threadMXBean != null && threadMXBean.isThreadAllocatedMemorySupported()) {
            threadMXBean.setThreadAllocatedMemoryEnabled(true);
        }
    }

    public void startMonitor() {
        resetForNewExecution();
    }

    public void resetForNewExecution() {
        this.instructionCount = 0;
        this.startTime = System.nanoTime();
        if (threadMXBean != null && threadMXBean.isThreadAllocatedMemoryEnabled()) {
            this.startAllocatedBytes = threadMXBean.getThreadAllocatedBytes(Thread.currentThread().getId());
        } else {
            this.startAllocatedBytes = -1L;
        }
        this.interrupted = false;
    }

    public void setLimits(int maxInstructions, long maxTimeMillis, long maxAllocatedBytes) {
        this.maxInstructions = maxInstructions;
        this.maxNanos = maxTimeMillis >= Long.MAX_VALUE / 1_000_000L ? Long.MAX_VALUE : maxTimeMillis * 1_000_000L;
        this.maxAllocatedBytes = maxAllocatedBytes;
    }

    @Override
    public void onInstruction(int pc, Varargs varargs, int top) {
        if (interrupted) {
            interrupted = false;
            throw new LuaError("Execution interrupted due to timeout!");
        }

        instructionCount++;
        if (instructionCount > maxInstructions) {
            throw new LuaError(
                    String.format(
                            "Execution Timeout: Script exceeded the maximum allowed instructions (%d steps).",
                            maxInstructions
                    )
            );
        }

        if (instructionCount % 100 == 0) {
            long elapsed = System.nanoTime() - startTime;
            if (elapsed > maxNanos) {
                throw new LuaError(
                        String.format(
                                "Execution Timeout: Script took too long to execute (limit is %.1f ms).",
                                maxNanos / 1000000.0
                        )
                );
            }
        }

        if (threadMXBean != null && startAllocatedBytes != -1L && instructionCount % 1000 == 0) {
            long currentAllocatedBytes = threadMXBean.getThreadAllocatedBytes(Thread.currentThread().getId());
            long allocatedDuringTask = currentAllocatedBytes - startAllocatedBytes;
            if (allocatedDuringTask > maxAllocatedBytes) {
                throw new LuaError(
                        "OOM Exception: Script exceeded memory allocation limit ("
                                + (maxAllocatedBytes / 1024 / 1024)
                                + "MB)."
                );
            }
        }
    }

    public void interrupt() {
        interrupted = true;
    }
}
