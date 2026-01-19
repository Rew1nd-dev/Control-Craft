package com.verr1.controlcraft.mixin;

import com.verr1.controlcraft.foundation.camera.CameraBoundFakePlayer;
import com.verr1.controlcraft.unstable.blocks.AiBoundFakePlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.phys.AABB;
import net.minecraftforge.common.util.FakePlayer;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(Entity.class)
public class MixinPlayer {

    @Inject(method = "getBoundingBox", at = @At("HEAD"), cancellable = true)
    void nonCollision0(CallbackInfoReturnable<AABB> cir){
        Entity self = Entity.class.cast(this);
        if(self instanceof CameraBoundFakePlayer || self instanceof AiBoundFakePlayer){
            cir.setReturnValue(new AABB(0.0, 0.0, 0.0, 0.0, 0.0, 0.0).move(self.position()));
        }
    }

    @Inject(method = "getBoundingBoxForCulling", at = @At("HEAD"), cancellable = true)
    void nonCollision1(CallbackInfoReturnable<AABB> cir){
        Entity self = Entity.class.cast(this);
        if(self instanceof CameraBoundFakePlayer || self instanceof AiBoundFakePlayer){
            cir.setReturnValue(new AABB(0.0, 0.0, 0.0, 0.0, 0.0, 0.0).move(self.position()));
        }
    }

    @Inject(method = "getBoundingBoxForPose", at = @At("HEAD"), cancellable = true)
    void nonCollision2(CallbackInfoReturnable<AABB> cir){
        Entity self = Entity.class.cast(this);
        if(self instanceof CameraBoundFakePlayer || self instanceof AiBoundFakePlayer){
            cir.setReturnValue(new AABB(0.0, 0.0, 0.0, 0.0, 0.0, 0.0).move(self.position()));
        }
    }
}
