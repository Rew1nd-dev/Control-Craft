package com.verr1.controlcraft.mixin.camera;


import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.managers.ClientCameraManager;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.ClientChunkCache;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.ChunkPos;
import net.minecraft.world.phys.Vec3;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.ModifyArg;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(ClientChunkCache.Storage.class)
public class MixinStorage {
    /*
    @Shadow @Final
    int chunkRadius;

    // @Inject(method = "inRange", at = @At(value = "HEAD"), cancellable = true)
    private void modifyInRange(int x, int z, CallbackInfoReturnable<Boolean> cir){
        // ControlCraft.LOGGER.info("Trying Drop: {} {}", x, z);
        Player player = Minecraft.getInstance().player;
        Vec3 latestWorldPos = ClientCameraManager.latestCameraWorldPos();
        Vec3 queryWorldPos = ClientCameraManager.QueryPos();
        if(latestWorldPos == null && queryWorldPos == null || player == null) return;

        Vec3 centerVec = latestWorldPos != null ? latestWorldPos : queryWorldPos;

        ChunkPos c_center = new ChunkPos(BlockPos.containing(centerVec));
        ChunkPos p_center = player.chunkPosition();

        if(     Math.abs(c_center.x - x) <= chunkRadius && Math.abs(c_center.z - z) <= chunkRadius
                ||
                Math.abs(p_center.x - x) <= chunkRadius && Math.abs(p_center.z - z) <= chunkRadius
        ) {
            // ControlCraft.LOGGER.info("Modified In Range Call: {} {}", x, z);
            cir.setReturnValue(true);
        }
    }
    * */
}
