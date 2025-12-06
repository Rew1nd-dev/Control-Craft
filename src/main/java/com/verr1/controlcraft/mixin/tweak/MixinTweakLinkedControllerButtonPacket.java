package com.verr1.controlcraft.mixin.tweak;

import com.getitemfromblock.create_tweaked_controllers.controller.ControllerRedstoneOutput;
import com.getitemfromblock.create_tweaked_controllers.packet.TweakedLinkedControllerButtonPacket;
import com.verr1.controlcraft.content.compact.tweak.TweakControllerServerRecorder;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.item.ItemStack;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Pseudo;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import java.util.List;

@Pseudo
@Mixin(TweakedLinkedControllerButtonPacket.class)
public class MixinTweakLinkedControllerButtonPacket {
    @Unique
    private static final ControllerRedstoneOutput controlCraft$UTIL = new ControllerRedstoneOutput();

    @Shadow(remap = false) private short buttonStates;

    @Inject(method = "handleItem", at = @At("HEAD"), remap = false)
    void saveToRecorder(ServerPlayer player, ItemStack heldItem, CallbackInfo ci){
         controlCraft$UTIL.DecodeButtons(buttonStates);
         TweakControllerServerRecorder.receiveButtons(player.getUUID(), List.of(controlCraft$UTIL.buttons));
    }

}
