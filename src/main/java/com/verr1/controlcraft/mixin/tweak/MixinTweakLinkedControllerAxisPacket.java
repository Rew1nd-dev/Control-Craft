package com.verr1.controlcraft.mixin.tweak;


import com.getitemfromblock.create_tweaked_controllers.packet.TweakedLinkedControllerAxisPacket;
import com.llamalad7.mixinextras.injector.wrapoperation.Operation;
import com.llamalad7.mixinextras.injector.wrapoperation.WrapOperation;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.LevelAccessor;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Pseudo;
import org.spongepowered.asm.mixin.injection.At;

import java.util.*;

@Pseudo
@Mixin(TweakedLinkedControllerAxisPacket.class)
public class MixinTweakLinkedControllerAxisPacket {


//    @WrapOperation(
//            method = "handleItem",
//            at = @At(
//                    value = "INVOKE",
//                    target = "Lcom/getitemfromblock/create_tweaked_controllers/controller/TweakedLinkedControllerServerHandler;ReceiveAxis(Lnet/minecraft/world/level/LevelAccessor;Lnet/minecraft/core/BlockPos;Ljava/util/UUID;Ljava/util/ArrayList;Ljava/util/ArrayList;)V"),
//            remap = false
//    )
//    void saveToRecorder(LevelAccessor entryA, BlockPos iterator, UUID entryB, ArrayList<Couple<RedstoneLinkNetworkHandler.Frequency>> i, ArrayList<Byte> world, Operation<Void> original){
//        original.call(entryA, iterator, entryB, i, world);
//        TweakControllerServerRecorder.receiveAxis(entryB, world.stream().map(Byte::doubleValue).toList());
//    }

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
