package com.verr1.controlcraft.mixin.tweak;

import com.getitemfromblock.create_tweaked_controllers.controller.TweakedControlsUtil;
import com.getitemfromblock.create_tweaked_controllers.controller.TweakedLinkedControllerClientHandler;
import com.getitemfromblock.create_tweaked_controllers.packet.TweakedLinkedControllerAxisPacket;
import com.llamalad7.mixinextras.sugar.Local;
import com.verr1.controlcraft.foundation.network.packets.specific.tweak.TweakControllerFullAxisPacket;
import com.verr1.controlcraft.registry.ControlCraftPackets;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

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
    private static void sendFullPrecision(CallbackInfo ci, @Local boolean useFullPrec){
        TweakedControlsUtil.Update(true);
        ControlCraftPackets.getChannel().sendToServer(new TweakControllerFullAxisPacket(TweakedControlsUtil.output.fullAxis));
        TweakedControlsUtil.Update(useFullPrec);
    }

}
