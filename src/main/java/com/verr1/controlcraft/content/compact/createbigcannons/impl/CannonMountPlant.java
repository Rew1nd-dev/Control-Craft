package com.verr1.controlcraft.content.compact.createbigcannons.impl;

import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.foundation.cimulink.game.peripheral.MutablePlant;
import com.verr1.controlcraft.mixinducks.ICannonDuck;
import org.jetbrains.annotations.NotNull;

public class CannonMountPlant extends MutablePlant {

    private boolean latestValue = false;

    protected CannonMountPlant(@NotNull ICannonDuck cannon) {
        super(new builder()
                .out("pitch", ($) -> (double)cannon.controlCraft$getPitch())
                .out("yaw", ($) -> (double)cannon.controlCraft$getYaw())
                .in("fire", (self, v) -> handleFire((CannonMountPlant) self, cannon, v))
        );
    }


    public void fire(ICannonDuck cannon, boolean powerChanged, boolean shouldFire){
        if(ControlCraftServer.onMainThread()){
            cannon.controlCraft$fire(shouldFire ? 15 : 0, powerChanged);
        }else{
            ControlCraftServer.SERVER_EXECUTOR.executeIfAbsent(cannon.controlCraft$getBlockPos().toShortString(), () -> fire(cannon, powerChanged, shouldFire));
        }
    }

    public static void handleFire(CannonMountPlant self, ICannonDuck cannon, double input){
        boolean current = input > 0.5;
        self.fire(cannon, current != self.latestValue, current);
        self.latestValue = current;
    }
}
