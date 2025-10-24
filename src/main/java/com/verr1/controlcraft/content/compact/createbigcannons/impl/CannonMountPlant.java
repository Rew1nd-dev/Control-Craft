package com.verr1.controlcraft.content.compact.createbigcannons.impl;

import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.foundation.cimulink.game.peripheral.MutablePlant;
import com.verr1.controlcraft.mixinducks.ICannonDuck;
import org.jetbrains.annotations.NotNull;

public class CannonMountPlant extends MutablePlant {

    private boolean latestValue = false;
    private boolean latestSample = false;
    private final ICannonDuck cached;

    protected CannonMountPlant(@NotNull ICannonDuck cannon) {
        super(new builder()
                .out("pitch", ($) -> (double)cannon.controlCraft$getPitch())
                .out("yaw", ($) -> (double)cannon.controlCraft$getYaw())
                .in("fire", (self, v) -> ((CannonMountPlant) self).latestSample = v > 0.5)
        );
        this.cached = cannon;
    }

    // Runs At Main Thread:
    // Run immediately if already at main thread.
    // Run At Physics Thread:
    // lastSample updates multiple times before scheduled task runs.
    // When scheduled task runs, it compares lastSample with lastValue to determine if power changed.
    // The scheduled task is unique since if there is already a scheduled task for the same cannon position, It will not schedule another one, before it is polled.
    public static void handleFire(CannonMountPlant self, ICannonDuck cannon){
        self.scheduleTick(
                () -> {
                    boolean powerChanged = self.latestSample != self.latestValue;
                    boolean shouldFire = self.latestSample;
                    self.latestValue = self.latestSample;
                    cannon.controlCraft$fire(shouldFire ? 15 : 0, powerChanged);
                },
                cannon.controlCraft$getBlockPos().toShortString()
        );
    }

    public void scheduleTick(Runnable task, String token){
        if(ControlCraftServer.onMainThread()){
            task.run();
        }else{
            ControlCraftServer.SERVER_EXECUTOR.executeIfAbsent(token, task);
        }
    }

    @Override
    protected void postPositiveEdge() {
        super.postPositiveEdge();
        handleFire(this, cached);
    }

//    public void fire(ICannonDuck cannon, boolean powerChanged, boolean shouldFire){
//        if(ControlCraftServer.onMainThread()){
//            cannon.controlCraft$fire(shouldFire ? 15 : 0, powerChanged);
//        }else{
//            ControlCraftServer.SERVER_EXECUTOR.execute(() -> fire(cannon, powerChanged, shouldFire));
//        }
//    }
//
//    public static void handleFire(CannonMountPlant self, ICannonDuck cannon, double input){
//        boolean current = input > 0.5;
//        self.fire(cannon, current != self.latestValue, current);
//        self.latestValue = current;
//    }
}
