package com.verr1.controlcraft.content.cctweaked.delegation;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.config.BlockPropertyConfig;
import dan200.computercraft.shared.computer.core.ServerContext;
import dan200.computercraft.shared.peripheral.monitor.MonitorWatcher;
import dan200.computercraft.shared.util.TickScheduler;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicReference;

public class ComputerCraftAsyncDelegation {
    private static final ExecutorService executor = Executors.newSingleThreadExecutor();
    private static final AtomicReference<Future<?>> currentTask = new AtomicReference<>();

    public static void onPhysTick() {
        if(!BlockPropertyConfig._CC_OVERCLOCKING)return;
        // 检查前一次任务是否完成
        Future<?> task = currentTask.get();
        if (task != null && !task.isDone()) {
            return; // 任务未完成，跳过本次tick
        }

        // 提交新任务
        Future<?> newTask = executor.submit(ComputerCraftAsyncDelegation::delegateRun);
        currentTask.set(newTask);
    }

    private static void delegateRun() {
        if(!BlockPropertyConfig._CC_OVERCLOCKING)return;
        try{
            // ControlCraft.LOGGER.info("log from another thread!");
            ServerContext.get(ControlCraftServer.INSTANCE).tick();
            TickScheduler.tick();
            MonitorWatcher.onTick();
        }catch (IllegalStateException e){
            ControlCraft.LOGGER.info("IllegalStateException caught: " + e.getMessage());
        }
    }

}
