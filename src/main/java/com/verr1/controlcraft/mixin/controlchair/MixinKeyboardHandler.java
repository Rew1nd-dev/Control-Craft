package com.verr1.controlcraft.mixin.controlchair;

import com.mojang.blaze3d.platform.InputConstants;
import com.verr1.controlcraft.foundation.input.ControlChairClientInput;
import com.verr1.controlcraft.foundation.managers.ClientControlChairManager;
import com.verr1.controlcraft.registry.ControlCraftAllKeyBindings;
import net.minecraft.client.KeyboardHandler;
import net.minecraft.client.Minecraft;
import org.lwjgl.glfw.GLFW;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(KeyboardHandler.class)
public class MixinKeyboardHandler {

    @Inject(method = "keyPress", at = @At("HEAD"), cancellable = true)
    private void controlCraft$captureChairKeyboard(
            long windowPointer,
            int key,
            int scanCode,
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
        InputConstants.Key inputKey = InputConstants.getKey(key, scanCode);
        if (key == GLFW.GLFW_KEY_ESCAPE
                || minecraft.options.keyTogglePerspective.isActiveAndMatches(inputKey)
                || ControlCraftAllKeyBindings.LINK_LAST_CAMERA.get().isActiveAndMatches(inputKey)
                || ClientControlChairManager.isReservedKey(key, scanCode)) {
            return;
        }

        boolean pressed = action != GLFW.GLFW_RELEASE;
        ControlChairClientInput.onCapturedKeyInput(key, pressed);
        ci.cancel();
    }
}
