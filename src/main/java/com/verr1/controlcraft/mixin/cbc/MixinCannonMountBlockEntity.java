package com.verr1.controlcraft.mixin.cbc;


import com.simibubi.create.content.contraptions.AssemblyException;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.mixinducks.ICannonDuck;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import rbasamoyai.createbigcannons.cannon_control.cannon_mount.CannonMountBlockEntity;
import rbasamoyai.createbigcannons.cannon_control.contraption.AbstractMountedCannonContraption;
import rbasamoyai.createbigcannons.cannon_control.contraption.PitchOrientedContraptionEntity;

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

    @Shadow(remap = false)
    protected abstract void assemble() throws AssemblyException;

    @Shadow(remap = false)
    protected PitchOrientedContraptionEntity mountedContraption;

    @Shadow(remap = false)
    public abstract void disassemble();

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

    private void tryAssemble(){
        try {
            assemble();
        } catch (AssemblyException ignored) {

        }
    }

    private void tryDisassemble(){
        disassemble();
    }

    @Override
    public void controlCraft$assemble(){
        if(ControlCraftServer.onMainThread()){
            tryAssemble();
        }else{
            ControlCraftServer.SERVER_EXECUTOR.executeLater(this::tryAssemble, 1);
        }
    }

    @Override
    public void controlCraft$disassemble() {
        if(ControlCraftServer.onMainThread()){
            tryDisassemble();
        }else{
            ControlCraftServer.SERVER_EXECUTOR.executeLater(this::tryDisassemble, 1);
        }
    }

    @Override
    public void controlCraft$fire() {
        // ((AbstractMountedCannonContraption)mountedContraption.getContraption()).fireShot();
    }
}
