package com.verr1.controlcraft.mixin.tweak;

import com.getitemfromblock.create_tweaked_controllers.block.TweakedLecternControllerBlockEntity;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.common.ForgeMod;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.valkyrienskies.mod.common.VSGameUtilsKt;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

@Mixin(TweakedLecternControllerBlockEntity.class)
public class MixinTweakLecternRangeSecond {

/*
@Redirect(
            method = "tryStartUsing",
            at = @At(value = "INVOKE", target = "Lcom/getitemfromblock/create_tweaked_controllers/block/TweakedLecternControllerBlockEntity;playerInRange(Lnet/minecraft/world/entity/player/Player;Lnet/minecraft/world/level/Level;Lnet/minecraft/core/BlockPos;)Z"),
            remap = false
    )
    boolean correctMountedDistanceCheck(Player player, Level world, BlockPos pos){

        var data = VSGameUtilsKt.getShipMountedToData(player, 1f);

        if(world.isClientSide || data == null){
            return TweakedLecternControllerBlockEntity.playerInRange(player, world, pos);
        }

        var mPos = data.getMountPosInShip();
        double reach = 0.4 * player.getAttributeValue(ForgeMod.BLOCK_REACH.get());
        return toMinecraft(mPos).distanceToSqr(Vec3.atCenterOf(pos)) < reach * reach;
    }
* */


}
