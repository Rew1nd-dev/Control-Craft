package com.verr1.controlcraft.mixin.tweak;

import com.getitemfromblock.create_tweaked_controllers.input.MouseCursorHandler;
import com.verr1.controlcraft.content.compact.tweak.TweakMouseLockState;
import com.verr1.controlcraft.utils.VSAccessUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.client.player.LocalPlayer;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(MouseCursorHandler.class)
public class MixinMouseCursorHandler {

    @Inject(method = "CancelPlayerTurn", at = @At("HEAD"), cancellable = true, remap = false)
    private static void controlCraft$stabilizeMountedView(CallbackInfo ci) {
        LocalPlayer player = Minecraft.getInstance().player;
        if (player == null || !TweakMouseLockState.isActive()) return;
        if (VSAccessUtils.getShipMountedTo(player) == null) return;

        float lockedPitch = TweakMouseLockState.lockedPitch();
        float lockedYaw = TweakMouseLockState.lockedYaw();

        player.setXRot(lockedPitch);
        player.setYRot(lockedYaw);
        player.xRotO = lockedPitch;
        player.yRotO = lockedYaw;
        player.yHeadRot = lockedYaw;
        player.yHeadRotO = lockedYaw;
        player.yBodyRot = lockedYaw;
        player.yBodyRotO = lockedYaw;
        player.xBob = lockedPitch;
        player.yBob = lockedYaw;
        player.xBobO = lockedPitch;
        player.yBobO = lockedYaw;
        ci.cancel();
    }

    @Inject(method = "ActivateMouseLock", at = @At("TAIL"), remap = false)
    private static void controlCraft$captureLockedView(CallbackInfo ci) {
        LocalPlayer player = Minecraft.getInstance().player;
        if (player == null) {
            TweakMouseLockState.deactivate();
            return;
        }

        TweakMouseLockState.activate(player.getXRot(), player.getYRot());
    }

    @Inject(method = "DeactivateMouseLock", at = @At("TAIL"), remap = false)
    private static void controlCraft$clearLockedView(CallbackInfo ci) {
        TweakMouseLockState.deactivate();
    }
}
