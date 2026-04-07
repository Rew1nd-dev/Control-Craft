package com.verr1.controlcraft.mixin.tweak;

import com.getitemfromblock.create_tweaked_controllers.controller.TweakedControlsUtil;
import com.getitemfromblock.create_tweaked_controllers.controller.TweakedLinkedControllerClientHandler;
import com.verr1.controlcraft.foundation.network.packets.specific.tweak.TweakControllerFullAxisPacket;
import com.verr1.controlcraft.registry.ControlCraftPackets;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(TweakedLinkedControllerClientHandler.class)
public class MixinTweakedControllerClientHandler {


    @Inject(
            method = "tick",
            at = @At(
                    value = "INVOKE",
                    target = "Lcom/getitemfromblock/create_tweaked_controllers/controller/ControllerRedstoneOutput;EncodeAxis()I"
            ),
            remap = false
    )
    private static void sendFullPrecision(CallbackInfo ci) {
        ControlCraftPackets.getChannel().sendToServer(new TweakControllerFullAxisPacket(controlCraft$collectFullAxis()));
    }

    @Unique
    private static float[] controlCraft$collectFullAxis() {
        float[] fullAxis = new float[6];
        for (int i = 0; i < fullAxis.length; i++) {
            float axisValue = TweakedControlsUtil.profile.GetAxis(i);
            if (i >= 4) {
                fullAxis[i] = Math.max(0.0F, Math.min((axisValue + 1.0F) * 0.5F, 1.0F));
            } else {
                fullAxis[i] = Math.max(-1.0F, Math.min(axisValue, 1.0F));
            }
        }
        return fullAxis;
    }

}
