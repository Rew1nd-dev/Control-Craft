package com.verr1.controlcraft.mixin.cbc;


import com.verr1.controlcraft.mixinducks.ICannonDuck;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import rbasamoyai.createbigcannons.cannon_control.cannon_mount.CannonMountBlockEntity;

@Mixin(CannonMountBlockEntity.class)
public abstract class MixinCannonMountBlockEntity implements ICannonDuck {

    @Shadow(remap = false)
    private float cannonYaw;

    @Shadow(remap = false)
    private float cannonPitch;


    @Shadow(remap = false)
    public abstract void setYaw(float yaw);

    @Shadow(remap = false)
    public abstract void setPitch(float pitch);

    public void controlCraft$setYaw(float value) {
        setYaw(value);
    }

    @Override
    public float controlCraft$getYaw() {
        return cannonYaw;
    }

    @Override
    public void controlCraft$setPitch(float value) {
        setPitch(value);
    }

    @Override
    public float controlCraft$getPitch() {
        return cannonPitch;
    }
}
