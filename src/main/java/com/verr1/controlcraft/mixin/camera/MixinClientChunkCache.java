package com.verr1.controlcraft.mixin.camera;


import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.camera.CameraClientChunkCacheExtension;
import com.verr1.controlcraft.foundation.managers.ClientCameraManager;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.ClientChunkCache;
import net.minecraft.client.multiplayer.ClientLevel;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.protocol.game.ClientboundLevelChunkPacketData;
import net.minecraft.world.level.ChunkPos;
import net.minecraft.world.level.chunk.ChunkStatus;
import net.minecraft.world.level.chunk.LevelChunk;
import net.minecraft.world.phys.Vec3;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import java.util.function.Consumer;

@Mixin(ClientChunkCache.class)
public class MixinClientChunkCache {

    @Shadow
    ClientChunkCache.Storage storage;


    @Shadow @Final
    ClientLevel level;

    @Inject(method = "replaceWithPacketData", at = @At("HEAD"), cancellable = true)
    private void onReplaceWithPacketData(
            int x, int z,
            FriendlyByteBuf buffer,
            CompoundTag chunkTag,
            Consumer<ClientboundLevelChunkPacketData.BlockEntityTagOutput> tagOutputConsumer,
            CallbackInfoReturnable<LevelChunk> cir
    ) {
        ControlCraft.LOGGER.info("Trying Replace: {} {}", x, z);

        int renderDistance = Minecraft.getInstance().options.renderDistance().get();
        ChunkPos pos = new ChunkPos(x, z);
        boolean isInPlayerRange = storage.inRange(x, z);
        boolean shouldAddChunk = false;

        Vec3 cameraPos = ClientCameraManager.getLinkOrQueryCameraWorldPosition();
        if(cameraPos == null)return;
        // Is Not Querying Or Linking
        if (pos.getChessboardDistance(new ChunkPos(BlockPos.containing(cameraPos))) <= (renderDistance + 1)){
            ControlCraft.LOGGER.info("Should Add Chunk: {} {}", x, z);
            shouldAddChunk = true;
        }

        if (shouldAddChunk) {
            LevelChunk newChunk = CameraClientChunkCacheExtension.replaceWithPacketData(level, x, z, new FriendlyByteBuf(buffer.copy()), chunkTag, tagOutputConsumer);

            if (!isInPlayerRange){
                ControlCraft.LOGGER.info("Adding Chunk: {} {}", x, z);
                cir.setReturnValue(newChunk);
            }

        }
    }

    @Inject(method = "drop", at = @At(value = "HEAD"))
    private void drop(int x, int z, CallbackInfo ci){


        /*
        Player player = Minecraft.getInstance().player;
        Vec3 latestWorldPos = ClientCameraManager.latestCameraWorldPos();
        if(latestWorldPos == null || player == null) return;


        ChunkPos c_center = new ChunkPos(BlockPos.containing(latestWorldPos));
        ChunkPos p_center = player.chunkPosition();

        if(     Math.abs(c_center.x - x) <= 2 && Math.abs(c_center.z - z) <= 2
                ||
                Math.abs(p_center.x - x) <= 2 && Math.abs(p_center.z - z) <= 2
        ) {
            ControlCraft.LOGGER.info("Drop Should Be Cancelled: {} {}", x, z);
            ci.cancel();
        }
        * */
        ChunkPos pos = new ChunkPos(x, z);
        int renderDistance = Minecraft.getInstance().options.renderDistance().get();

        Vec3 cameraPos = ClientCameraManager.getLinkOrQueryCameraWorldPosition();
        if(cameraPos == null)return;
        // Player Is Linked And Game Is Request to Drop A Chunk Inside Camera's View, Don't Drop It From Extension
        if (pos.getChessboardDistance(new ChunkPos(BlockPos.containing(cameraPos))) <= (renderDistance + 1))
            return;

        ControlCraft.LOGGER.info("Trying Drop Extension: {} {}", x, z);

        CameraClientChunkCacheExtension.drop(level, pos);
    }

    @Inject(method = "getChunk(IILnet/minecraft/world/level/chunk/ChunkStatus;Z)Lnet/minecraft/world/level/chunk/LevelChunk;", at = @At("TAIL"), cancellable = true)
    private void getChunk(
            int x,
            int z,
            ChunkStatus requiredStatus,
            boolean requireChunk,
            CallbackInfoReturnable<LevelChunk> cir
    ){

        if (!storage.inRange(x, z)) {
            LevelChunk chunk = CameraClientChunkCacheExtension.getChunk(x, z);

            if (chunk != null){
                // ControlCraft.LOGGER.info("Returning Chunk: {} {}", x, z);
                cir.setReturnValue(chunk);
            }
        }
    }

    /*
    @Inject(method="calculateStorageRange", at = @At(value = "HEAD"), cancellable = true)
    private static void modifyStorageRange(int r, CallbackInfoReturnable<Integer> cir){
        cir.setReturnValue(Math.max(2, 4 * r) + 3);
    }
    * */


}
