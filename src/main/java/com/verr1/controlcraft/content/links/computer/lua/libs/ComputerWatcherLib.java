package com.verr1.controlcraft.content.links.computer.lua.libs;

import org.luaj.vm2.LuaError;
import org.luaj.vm2.Varargs;
import org.luaj.vm2.lib.DebugLib;

import java.lang.management.ManagementFactory;
import com.sun.management.ThreadMXBean;

public class ComputerWatcherLib extends DebugLib {

    // 默认每个 Tick 允许执行的最大 Lua 字节码指令数 (防止死循环)
    // 根据实际测试，一般 10000 步指令在主线程耗时约为 0.1ms 到 0.5ms，不会引起明显卡顿。
    private int maxInstructions = 10000;

    // 记录在本次 execution context 中已经执行的指令数
    private int instructionCount = 0;

    // (可选) 也可同时开启时间检测熔断，单位纳秒
    private long maxNanos = 2_000_000L; // 2 毫秒
    private long startTime = 0;

    // --- OOM 保护相关 ---
    private final ThreadMXBean threadMXBean;
    private long startAllocatedBytes = -1;
    private long maxAllocatedBytes = 5 * 1024 * 1024L; // 默认 5MB 的临时分配上限

    public ComputerWatcherLib() {
        super();
        this.threadMXBean = (ThreadMXBean) ManagementFactory.getThreadMXBean();
        // 如果运行环境 (JVM) 支持统计线程内存分配，则开启它
        if (threadMXBean != null && threadMXBean.isThreadAllocatedMemorySupported()) {
            threadMXBean.setThreadAllocatedMemoryEnabled(true);
        }
    }

    /**
     * 在每次准备在主线程（如 ServerTick / ClientTick）执行 Lua 脚本的某个函数前，
     * 必须调用此方法重置计数器！
     */
    public void resetForNewExecution() {
        this.instructionCount = 0;
        this.startTime = System.nanoTime();

        // 记录执行前的已分配内存游标
        if (threadMXBean != null && threadMXBean.isThreadAllocatedMemoryEnabled()) {
            this.startAllocatedBytes = threadMXBean.getThreadAllocatedBytes(Thread.currentThread().getId());
        } else {
            this.startAllocatedBytes = -1;
        }
    }

    /**
     * 可以让外部配置这台显示器的最大性能上限（比如创造模式允许更多步数）
     */
    public void setLimits(int maxInstructions, long maxTimeMillis, long maxMemoryBytes) {
        this.maxInstructions = maxInstructions;
        this.maxNanos = maxTimeMillis * 1_000_000L;
        this.maxAllocatedBytes = maxMemoryBytes;
    }

    @Override
    public void onInstruction(int pc, Varargs v, int top) {
        super.onInstruction(pc, v, top); // 如果父类有逻辑，先执行

        instructionCount++;

        // 1. 检查指令步数是否超标（极度廉价的检查）
        if (instructionCount > maxInstructions) {
            throw new LuaError(
                    String.format("Execution Timeout: Script exceeded the maximum allowed instructions (%d steps).",
                            maxInstructions));
        }

        // 2. 检查真实时间和 OOM，为了性能，每 100 步检查一次
        if (instructionCount % 100 == 0) {
            // 时间检查
            long elapsed = System.nanoTime() - startTime;
            if (elapsed > maxNanos) {
                throw new LuaError(
                        String.format("Execution Timeout: Script took too long to execute (limit is %.1f ms).",
                                maxNanos / 1000000.0));
            }

            // OOM 攻击检查：是否在短时间内狂刷字符串或 Table
            if (threadMXBean != null && startAllocatedBytes != -1) {
                long currentAllocatedBytes = threadMXBean.getThreadAllocatedBytes(Thread.currentThread().getId());
                long allocatedDuringTask = currentAllocatedBytes - startAllocatedBytes;

                if (allocatedDuringTask > maxAllocatedBytes) {
                    throw new LuaError(
                            String.format("OOM Exception: Script exceeded memory allocation limit (%.2f MB).",
                                    maxAllocatedBytes / 1024.0 / 1024.0));
                }
            }
        }
    }
}
