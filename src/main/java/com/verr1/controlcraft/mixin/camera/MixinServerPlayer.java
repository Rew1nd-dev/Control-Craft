package com.verr1.controlcraft.mixin.camera;

import com.verr1.controlcraft.foundation.managers.ServerCameraManager;
import com.verr1.controlcraft.mixinducks.IServerPlayerDuck;
import net.minecraft.core.SectionPos;
import net.minecraft.server.level.ServerPlayer;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(value = ServerPlayer.class, priority = 1400)
public class MixinServerPlayer implements IServerPlayerDuck {


    @Unique
    private SectionPos controlcraft$lastSectionPos = SectionPos.of(0, 0, 0);



    // @Redirect(method = "tick", at = @At(value = "INVOKE", target = "Lnet/minecraft/server/level/ServerPlayer;absMoveTo(DDDFF)V"))
    private void securitycraft$tick(ServerPlayer player, double x, double y, double z, float yaw, float pitch) {
        if (!ServerCameraManager.isRegistered(player.getUUID())) player.absMoveTo(x, y, z, yaw, pitch);
    }

    @Override
    public SectionPos controlcraft$lastSectionPos() {
        return controlcraft$lastSectionPos;
    }

    @Override
    public void controlcraft$setLastSectionPos(SectionPos pos) {
        this.controlcraft$lastSectionPos = pos;
    }

    @Override
    public SectionPos controlcraft$getAndSetLastSectionPos(SectionPos newPos) {
        SectionPos oldPos = this.controlcraft$lastSectionPos;
        this.controlcraft$lastSectionPos = newPos;
        return oldPos;
    }
}
