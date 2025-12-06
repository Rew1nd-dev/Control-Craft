package com.verr1.controlcraft.mixin.tweak;


import com.getitemfromblock.create_tweaked_controllers.block.TweakedLecternControllerBlock;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Pseudo;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

@Pseudo
@Mixin(TweakedLecternControllerBlock.class)
public class MixinTweakLecternRangeFirst {

/*
@Redirect(
            method = "use",
            at = @At(value = "INVOKE", deploy = "Lcom/getitemfromblock/create_tweaked_controllers/block/TweakedLecternControllerBlockEntity;playerInRange(Lnet/minecraft/world/entity/player/Player;Lnet/minecraft/world/level/Level;Lnet/minecraft/core/BlockPos;)Z"),
            remap = false
    )
    boolean avoidDistanceCheck(Player player, Level world, BlockPos pos){
        return true;
    }
* */


}
