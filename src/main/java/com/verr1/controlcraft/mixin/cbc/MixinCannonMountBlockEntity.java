package com.verr1.controlcraft.mixin.cbc;


import com.simibubi.create.content.contraptions.AbstractContraptionEntity;
import com.simibubi.create.content.contraptions.AssemblyException;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.mixinducks.ICannonDuck;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Pseudo;
import org.spongepowered.asm.mixin.Shadow;
import rbasamoyai.createbigcannons.cannon_control.cannon_mount.CannonMountBlockEntity;
import rbasamoyai.createbigcannons.cannon_control.contraption.AbstractMountedCannonContraption;
import rbasamoyai.createbigcannons.cannon_control.contraption.PitchOrientedContraptionEntity;

import javax.annotation.Nullable;
import java.util.Optional;

@Pseudo
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

    @Shadow(remap = false)
    @Nullable public abstract PitchOrientedContraptionEntity getContraption();


    @Shadow(remap = false)
    public abstract CannonMountBlockEntity getCannonMount();

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
    public void controlCraft$fire(int strength, boolean fireChanged){
        CannonMountBlockEntity self = getCannonMount();
        if(self == null)return;
        ServerLevel sLevel = (ServerLevel) self.getLevel();
        if(sLevel == null)return;
        Optional.ofNullable(mountedContraption)
                .map(AbstractContraptionEntity::getContraption)
                .filter(AbstractMountedCannonContraption.class::isInstance)
                .map(AbstractMountedCannonContraption.class::cast)
                .ifPresent(contraption -> contraption.onRedstoneUpdate(sLevel, this.mountedContraption, fireChanged, strength, self));

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
    public BlockPos controlCraft$getBlockPos() {
        return getCannonMount().getBlockPos();
    }
}
