package com.verr1.controlcraft.mixin.tweak;

import com.verr1.controlcraft.content.compact.tweak.TweakMouseLockState;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.world.entity.Entity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;
import org.valkyrienskies.mod.common.VSGameUtilsKt;

@Mixin(value = LocalPlayer.class, priority = 1500)
public abstract class MixinLocalPlayerMouseLockView {

    @Inject(method = "getViewYRot", at = @At("HEAD"), cancellable = true)
    private void controlCraft$useLockedYaw(float partialTick, CallbackInfoReturnable<Float> cir) {
        if (controlCraft$shouldUseLockedView()) {
            cir.setReturnValue(TweakMouseLockState.lockedYaw());
        }
    }

    @Inject(method = "getViewXRot", at = @At("HEAD"), cancellable = true)
    private void controlCraft$useLockedPitch(float partialTick, CallbackInfoReturnable<Float> cir) {
        if (controlCraft$shouldUseLockedView()) {
            cir.setReturnValue(TweakMouseLockState.lockedPitch());
        }
    }

    @Unique
    private boolean controlCraft$shouldUseLockedView() {
        if (!TweakMouseLockState.isActive()) return false;
        return VSGameUtilsKt.getShipMountedTo((Entity) (Object) this) != null;
    }
}