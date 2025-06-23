package com.verr1.controlcraft.mixin;


import com.simibubi.create.content.kinetics.speedController.SpeedControllerBlockEntity;
import com.simibubi.create.foundation.blockEntity.behaviour.scrollValue.ScrollValueBehaviour;
import com.verr1.controlcraft.mixinducks.ISpeedControllerDuck;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.Inject;


// unused
@Mixin(SpeedControllerBlockEntity.class)
public class MixinSpeedController implements ISpeedControllerDuck {

    @Shadow(remap = false) public ScrollValueBehaviour targetSpeed;
    @Unique
    private boolean controlCraft$hasNextTickSchedule = false;
    @Unique
    private int controlCraft$nextTickSpeed = 0;

    @Override
    public void controlCraft$scheduleNextTick(int speed) {
        controlCraft$hasNextTickSchedule = true;
        controlCraft$nextTickSpeed = speed;
    }

    @Unique
    private void runSchedule(){
        controlCraft$hasNextTickSchedule = false;
        targetSpeed.setValue(controlCraft$nextTickSpeed);
    }



    @Unique
    public void tick(){

    }

}
