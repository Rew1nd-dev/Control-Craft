package com.verr1.controlcraft.mixin;

import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.phys.AABB;
import net.minecraftforge.common.util.FakePlayer;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(Entity.class)
public class MixinPlayer {

    @Inject(method = "getBoundingBox", at = @At("HEAD"), cancellable = true)
    void nonCollision(CallbackInfoReturnable<AABB> cir){
        Entity self = Entity.class.cast(this);
        if(self instanceof FakePlayer){
            cir.setReturnValue(new AABB(0.0, 0.0, 0.0, 0.0, 0.0, 0.0));
        }
    }

}
