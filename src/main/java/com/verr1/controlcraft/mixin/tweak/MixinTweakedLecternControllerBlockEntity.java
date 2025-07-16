package com.verr1.controlcraft.mixin.tweak;


import com.getitemfromblock.create_tweaked_controllers.block.TweakedLecternControllerBlockEntity;
import com.verr1.controlcraft.config.BlockPropertyConfig;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(TweakedLecternControllerBlockEntity.class)
public class MixinTweakedLecternControllerBlockEntity {

/*
@Inject(method = "shouldUseFullPrecision", at = @At("HEAD"), remap = false, cancellable = true)
    void controlCraft$shouldUseFullPrecision(CallbackInfoReturnable<Boolean> cir){
        if(!BlockPropertyConfig._TWEAKED_CONTROLLER_256)return;
        cir.setReturnValue(true);
        cir.cancel();
    }
* */


}
