package com.verr1.controlcraft.mixin.qol;


import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory;
import com.verr1.gearwork.content.landinggear.LandingGearBlock;
import net.minecraft.core.BlockPos;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(LandingGearBlock.class)
public class MixinGearBlock {

    @Inject(method = "displayScreen", at = @At("HEAD"), remap = false)
    void openScreen(BlockPos p, CallbackInfo ci){
        ScreenOpener.open(CimulinkUIFactory.createNameOnlyScreen(p));
    }



}
