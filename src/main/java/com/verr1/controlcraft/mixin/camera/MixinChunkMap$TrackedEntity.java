package com.verr1.controlcraft.mixin.camera;


import com.verr1.controlcraft.foundation.managers.ServerCameraManager;
import net.minecraft.server.level.ChunkMap;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.phys.Vec3;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;


@Mixin(ChunkMap.TrackedEntity.class)
public class MixinChunkMap$TrackedEntity {

    @Redirect(method = "updatePlayer", at = @At(value = "INVOKE", target = "Lnet/minecraft/server/level/ServerPlayer;position()Lnet/minecraft/world/phys/Vec3;"))
    private Vec3 wwa(ServerPlayer player) {
        if (ServerCameraManager.isRegistered(player.getUUID())) {
            return ServerCameraManager.getCachedCameraOrPlayerPosition(player);
        }
        return player.position();
    }

}
