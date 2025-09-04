package com.verr1.controlcraft.mixin.camera;


import com.llamalad7.mixinextras.injector.wrapoperation.WrapOperation;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.config.BlockPropertyConfig;
import com.verr1.controlcraft.foundation.camera.CameraClientChunkCacheExtension;
import com.verr1.controlcraft.foundation.managers.ClientCameraManager;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.ClientChunkCache;
import net.minecraft.client.multiplayer.ClientLevel;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.protocol.game.ClientboundLevelChunkPacketData;
import net.minecraft.server.level.ChunkMap;
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

import javax.annotation.Nullable;
import java.util.function.Consumer;

import static com.verr1.controlcraft.utils.MinecraftUtils._isChunkInRange;
import static net.minecraft.server.level.ChunkMap.isChunkInRange;

@Mixin(ClientChunkCache.class)
public abstract class MixinClientChunkCache {

    @Shadow
    ClientChunkCache.Storage storage;


    @Shadow @Final
    ClientLevel level;

    private static boolean isValidChunk(@Nullable LevelChunk p_104439_, int x, int z) {
        if (p_104439_ == null) {
            return false;
        } else {
            ChunkPos chunkpos = p_104439_.getPos();
            return chunkpos.x == x && chunkpos.z == z;

        }
    }

    @Inject(method = "replaceWithPacketData", at = @At("HEAD"), cancellable = true)
    private void onReplaceWithPacketData(
            int x, int z,
            FriendlyByteBuf buffer,
            CompoundTag chunkTag,
            Consumer<ClientboundLevelChunkPacketData.BlockEntityTagOutput> tagOutputConsumer,
            CallbackInfoReturnable<LevelChunk> cir
    ) {
        // ControlCraft.LOGGER.debug("Trying Replace: {} {}", x, z);

        int renderDistance = Minecraft.getInstance().options.renderDistance().get();
        ChunkPos pos = new ChunkPos(x, z);
        boolean isInPlayerRange = storage.inRange(x, z);

        if(!isInPlayerRange){
            boolean shouldAddChunk = BlockPropertyConfig._ALWAYS_ADD_CAMERA_CHUNK; // always trust server ?

            Vec3 cameraPos = ClientCameraManager.getLinkOrQueryCameraWorldPosition();

            if(cameraPos == null)return;
            ChunkPos cPos = new ChunkPos(BlockPos.containing(cameraPos));
            // Is Not Querying Or Linking
            // isChunkInRange(pos.x, pos.z, cPos.x, cPos.z, renderDistance + 1)
            // _isChunkInRange(pos.x, pos.z, cPos.x, cPos.z, renderDistance + 1)
            // pos.getChessboardDistance(cPos) < renderDistance + 1
            if (isChunkInRange(pos.x, pos.z, cPos.x, cPos.z, 2 * renderDistance + 1)){
                // ControlCraft.LOGGER.info("Should Add Chunk: {} {} cam: {}", x, z, new ChunkPos(BlockPos.containing(cameraPos)));
                shouldAddChunk = true;
            }else{
                ControlCraft.LOGGER.debug("Chunk: {} {} Not In View Range: cam: {}", x, z, new ChunkPos(BlockPos.containing(cameraPos)));
            }

            if (shouldAddChunk) {
                LevelChunk newChunk = CameraClientChunkCacheExtension.replaceWithPacketData(level, x, z, new FriendlyByteBuf(buffer.copy()), chunkTag, tagOutputConsumer);
                // ControlCraft.LOGGER.debug("Adding Chunk: {} {}", x, z);
                cir.setReturnValue(newChunk);

            }
        }


    }



    @Inject(method = "drop", at = @At(value = "HEAD"))
    private void drop(int x, int z, CallbackInfo ci){
        ChunkPos pos = new ChunkPos(x, z);
        int renderDistance = Minecraft.getInstance().options.renderDistance().get();

        Vec3 cameraPos = ClientCameraManager.getLinkOrQueryCameraWorldPosition();
        if(cameraPos == null)return;

        // Don't Double-Check Whether To Drop, The Server-Side Drop Command Is Guaranteed To Be Correct, Ideally

        /*
        * if (pos.getChessboardDistance(new ChunkPos(BlockPos.containing(cameraPos))) <= (renderDistance + 1)){
            ControlCraft.LOGGER.info("Stop Dropping Extension: {} {}, Cam: {}", x, z, new ChunkPos(BlockPos.containing(cameraPos)));
            return;
        }
        *
        *
        * */


        // ControlCraft.LOGGER.debug("Drop Extension: {} {}", x, z);

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
        if (this.storage.inRange(x, z)) {
            LevelChunk levelchunk = this.storage.getChunk(this.storage.getIndex(x, z));
            if (isValidChunk(levelchunk, x, z)) {
                return;
            }
        }

        LevelChunk chunk = CameraClientChunkCacheExtension.getChunk(x, z);

        if (chunk != null){
            // ControlCraft.LOGGER.debug("Returning Chunk: {} {}", x, z);
            cir.setReturnValue(chunk);
        }else{
            // ControlCraft.LOGGER.debug("chunk {} {} is also null in extension", x, z);
        }
    }



    /*
    @Inject(method="calculateStorageRange", at = @At(value = "HEAD"), cancellable = true)
    private static void modifyStorageRange(int r, CallbackInfoReturnable<Integer> cir){
        cir.setReturnValue(Math.max(2, 4 * r) + 3);
    }
    * */


}
