package com.verr1.controlcraft.foundation.cimulink.core.components.lua;

import com.sun.management.ThreadMXBean;
import org.luaj.vm2.LuaError;
import org.luaj.vm2.Varargs;
import org.luaj.vm2.lib.DebugLib;

import java.lang.management.ManagementFactory;

public class WatcherLib extends DebugLib {

    private volatile boolean interrupted = false;
    private final ThreadMXBean threadMXBean;
    private long startAllocatedBytes = -1;
    // 假设设定每个被执行的脚本最多只能临时分配 10MB 的内存
    private final long maxAllocatedBytes = 10 * 1024 * 1024L;

    public WatcherLib() {
        this.threadMXBean = (ThreadMXBean) ManagementFactory.getThreadMXBean();
        // 如果环境支持，开启线程内存分配统计
        if (threadMXBean != null && threadMXBean.isThreadAllocatedMemorySupported()) {
            threadMXBean.setThreadAllocatedMemoryEnabled(true);
        }
    }
    // 在 Luacuit doTask 时调用，记录起始分配内存
    public void startMonitor() {
        if (threadMXBean != null && threadMXBean.isThreadAllocatedMemoryEnabled()) {
            this.startAllocatedBytes = threadMXBean.getThreadAllocatedBytes(Thread.currentThread().getId());
        }
        this.interrupted = false; // 重置中断标志
    }
    @Override
    public void onInstruction(int i, Varargs varargs, int i1) {
        if (interrupted) {
            interrupted = false;
            throw new LuaError("Execution interrupted due to timeout!"); // 超时导致的强制中断
        }

        // 性能优化：无需每执行 1 条指令看一次内存，每执行 1000 条检查一次即可
        if (threadMXBean != null && startAllocatedBytes != -1 && (i % 1000 == 0)) {
            long currentAllocatedBytes = threadMXBean.getThreadAllocatedBytes(Thread.currentThread().getId());
            long allocatedDuringTask = currentAllocatedBytes - startAllocatedBytes;
            // 检查本次脚本运行是否瞬间分配了巨量对象（例如恶意死循环拼接字符串）
            if (allocatedDuringTask > maxAllocatedBytes) {
                throw new LuaError("OOM Exception: Script exceeded memory allocation limit (" + (maxAllocatedBytes / 1024 / 1024) + "MB).");
            }
        }
    }
    public void interrupt() {
        interrupted = true;
    }

}
