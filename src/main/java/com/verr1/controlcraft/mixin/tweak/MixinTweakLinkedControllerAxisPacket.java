package com.verr1.controlcraft.mixin.tweak;


import com.getitemfromblock.create_tweaked_controllers.block.TweakedLecternControllerBlockEntity;
import com.getitemfromblock.create_tweaked_controllers.item.TweakedLinkedControllerItem;
import com.getitemfromblock.create_tweaked_controllers.packet.TweakedLinkedControllerAxisPacket;
import com.simibubi.create.content.redstone.link.RedstoneLinkNetworkHandler;
import com.simibubi.create.foundation.utility.Couple;
import com.verr1.controlcraft.config.BlockPropertyConfig;
import com.verr1.controlcraft.content.compact.tweak.impl.TweakedLinkedControllerServerHandlerExtension;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import java.util.*;

@Mixin(TweakedLinkedControllerAxisPacket.class)
public class MixinTweakLinkedControllerAxisPacket {
/*
@Shadow(remap = false)
    private float[] fullAxis;

    @Inject(method = "handleLectern", at = @At("HEAD"), remap = false)
    void controlCraft$handleLectern(ServerPlayer player, TweakedLecternControllerBlockEntity lectern, CallbackInfo ci){
        if(!BlockPropertyConfig._TWEAKED_CONTROLLER_256)return;
        if(!lectern.isUsedBy(player))return;

        lectern.SetFullPrecision(true); // If not, Lectern axis can't be read by cc

        ItemStack heldItem = lectern.getController().copy();
        Level world = player.getCommandSenderWorld();
        UUID uniqueID = player.getUUID();
        BlockPos pos = player.blockPosition();
        if(fullAxis == null)return;
        if (!player.isSpectator()) {
            ArrayList<Couple<RedstoneLinkNetworkHandler.Frequency>> axisCouples = new ArrayList<>(10);
            ArrayList<Float> axisValues = new ArrayList<>(controlCraft$makeAxis(fullAxis));

            for(byte i = 0; i < 10; ++i) {
                axisCouples.add(TweakedLinkedControllerItem.toFrequency(heldItem, i + 15));
            }

            TweakedLinkedControllerServerHandlerExtension.ReceiveAxis(world, pos, uniqueID, axisCouples, axisValues);
        }
    }


    private static List<Float> controlCraft$makeAxis(float[] axis){
        float _LX = axis[0] > 0 ? axis[0] : 0;
        float LX_ = axis[0] < 0 ? -axis[0] : 0;
        float _LY = axis[1] > 0 ? axis[1] : 0;
        float LY_ = axis[1] < 0 ? -axis[1] : 0;
        float _RX = axis[2] > 0 ? axis[2] : 0;
        float RX_ = axis[2] < 0 ? -axis[2] : 0;
        float _RY = axis[3] > 0 ? axis[3] : 0;
        float RY_ = axis[3] < 0 ? -axis[3] : 0;

        float LT = axis[4] > 0 ? axis[4] : 0;
        float RT = axis[5] < 0 ? -axis[4] : 0;

        return List.of(
                _LX, LX_, _LY, LY_, _RX, RX_, _RY, RY_, LT, RT
        );
    }
* */



}
