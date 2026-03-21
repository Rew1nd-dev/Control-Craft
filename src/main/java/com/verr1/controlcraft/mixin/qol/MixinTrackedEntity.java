package com.verr1.controlcraft.mixin.qol;

import com.verr1.controlcraft.hooks.NoCullEntityHooks;
import net.minecraft.server.level.ChunkMap;
import net.minecraft.world.entity.Entity;
import org.spongepowered.asm.mixin.*;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.valkyrienskies.core.impl.config.VSCoreConfig;

@Mixin(ChunkMap.TrackedEntity.class)
public class MixinTrackedEntity {

    @Mutable
    @Final
    @Shadow
    private int range;

    @Unique
    private boolean controlCraft$shouldNoCull;

    @Inject(
        method = "<init>(Lnet/minecraft/world/entity/Entity;IIZ)V",
        at = @At("TAIL")
    )
    private void adjustTrackingRangeForContraption(
        Entity entity,
        int range,
        int updateInterval,
        boolean trackDeltas,
        CallbackInfo ci
    ) {
        if (!NoCullEntityHooks.shouldNoCull(entity)) {
            return;
        }
        controlCraft$shouldNoCull = true;
        this.range = controlCraft$getDynamicTargetRange();
    }


    @Redirect(
        method = "updatePlayer",
        at = @At(
            value = "INVOKE",
            target = "Ljava/lang/Math;min(II)I"  // 注意：这里是 int min(int, int)
        )
    )
    private int overrideMinForContraption(int eff, int viewRange) {
        if (controlCraft$shouldNoCull) {
            return Math.max(eff, viewRange);
        }
        return Math.min(eff, viewRange);  // 原逻辑
    }


    @Unique
    private static int controlCraft$getDynamicTargetRange() {
        return (int) VSCoreConfig.SERVER.getShipUnloadDistance() + 16;
    }

}
