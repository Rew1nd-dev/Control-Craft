package com.verr1.controlcraft.mixin.camera;


import com.verr1.controlcraft.content.blocks.camera.CameraBlockEntity;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.managers.ServerCameraManager;
import com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies;
import net.minecraft.server.level.ChunkMap;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.phys.Vec3;
import org.objectweb.asm.Opcodes;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.ModifyVariable;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

import java.util.Optional;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

@Mixin(ChunkMap.TrackedEntity.class)
public class MixinChunkMap$TrackedEntity {

    @Redirect(method = "updatePlayer", at = @At(value = "INVOKE", target = "Lnet/minecraft/server/level/ServerPlayer;position()Lnet/minecraft/world/phys/Vec3;"))
    private Vec3 wwa(ServerPlayer player) {
        if (ServerCameraManager.isRegistered(player.getUUID())) {
            return Optional
                    .ofNullable(ServerCameraManager.getCamera(player))
                    .flatMap(p -> BlockEntityGetter.INSTANCE
                            .getBlockEntityAt(p.globalPos(), CameraBlockEntity.class)
                    )
                    .map(CameraBlockEntity::getCameraPosition)
                    .map(ValkyrienSkies::toMinecraft)
                    .orElse(player.position());

        }
        return player.position();
    }

}
