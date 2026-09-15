package com.verr1.controlcraft.mixin.controlchair;

import com.verr1.controlcraft.foundation.input.ControlChairClientInput;
import com.verr1.controlcraft.foundation.managers.ClientControlChairManager;
import net.minecraft.client.Minecraft;
import net.minecraft.client.MouseHandler;
import org.lwjgl.glfw.GLFW;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(MouseHandler.class)
public class MixinMouseHandler {

    @Inject(method = "onPress", at = @At("HEAD"), cancellable = true)
    private void controlCraft$captureChairMouseButtons(
            long windowPointer,
            int button,
            int action,
            int modifiers,
            CallbackInfo ci
    ) {
        Minecraft minecraft = Minecraft.getInstance();
        if (windowPointer != minecraft.getWindow().getWindow()) {
            return;
        }
        if (!ClientControlChairManager.isInputCaptured() || minecraft.screen != null) {
            return;
        }

        boolean pressed = action != GLFW.GLFW_RELEASE;
        ControlChairClientInput.onCapturedMouseButton(button, pressed);
        ci.cancel();
    }

    @Inject(method = "turnPlayer", at = @At("HEAD"), cancellable = true)
    private void controlCraft$captureChairMouseTurn(CallbackInfo ci) {
        if (!ClientControlChairManager.hasActiveChair()) {
            ControlChairClientInput.resetMouseTurnBaseline();
            return;
        }
        if (ClientControlChairManager.shouldCancelMouseTurn()) {
            MouseHandler self = (MouseHandler) (Object) this;
            ControlChairClientInput.captureLockedMouseTurn(self.getXVelocity(), self.getYVelocity());
            ClientControlChairManager.applyLockedView();
            ci.cancel();
            return;
        }
        ControlChairClientInput.resetMouseTurnBaseline();
    }
}
