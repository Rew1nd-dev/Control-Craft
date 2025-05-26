package com.verr1.controlcraft.mixin.camera;


import com.google.common.collect.ImmutableList;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.blocks.camera.CameraBlockEntity;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.managers.ServerCameraManager;
import com.verr1.controlcraft.mixinducks.IServerPlayerDuck;
import it.unimi.dsi.fastutil.ints.Int2ObjectMap;
import kotlin.Pair;
import net.minecraft.core.SectionPos;
import net.minecraft.network.protocol.game.ClientboundLevelChunkWithLightPacket;
import net.minecraft.network.protocol.game.DebugPackets;
import net.minecraft.server.level.ChunkHolder;
import net.minecraft.server.level.ChunkMap;
import net.minecraft.server.level.PlayerMap;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.ChunkPos;
import net.minecraft.world.level.chunk.LevelChunk;
import net.minecraft.world.phys.Vec3;
import org.apache.commons.lang3.mutable.MutableObject;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

import javax.annotation.Nullable;
import java.util.*;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;
import static net.minecraft.server.level.ChunkMap.isChunkInRange;

@Mixin(ChunkMap.class)
public abstract class MixinChunkMap {


    @Shadow
    int viewDistance;

    @Shadow protected abstract void updateChunkTracking(ServerPlayer p_183755_, ChunkPos p_183756_, MutableObject<ClientboundLevelChunkWithLightPacket> p_183757_, boolean p_183758_, boolean p_183759_);


    @Shadow @Final private Int2ObjectMap<ChunkMap.TrackedEntity> entityMap;

    @Shadow @Final private PlayerMap playerMap;

    @Shadow protected abstract boolean skipPlayer(ServerPlayer p_140330_);


    @Shadow @Nullable protected abstract ChunkHolder getVisibleChunkIfPresent(long p_140328_);

    @Shadow @Final private ChunkMap.DistanceManager distanceManager;


    private static boolean _isChunkInRange(int x1, int z1, int x2, int z2, int viewDistance){
        return new ChunkPos(x1, z1).getChessboardDistance(new ChunkPos(x2, z2)) < viewDistance;
    }

    @Inject(method = "updateChunkTracking", at = @At("HEAD"))
    private void onUpdateChunkTracking(
            ServerPlayer p_183755_,
            ChunkPos p_183756_,
            MutableObject<ClientboundLevelChunkWithLightPacket> p_183757_,
            boolean p_183758_,
            boolean p_183759_,
            CallbackInfo ci
    ){
        if (p_183759_ && !p_183758_) {
            ChunkHolder chunkholder = this.getVisibleChunkIfPresent(p_183756_.toLong());

            if (chunkholder == null) {
                // ControlCraft.LOGGER.info("ChunkHolder {} {} is null", p_183756_.x, p_183756_.z);
            }else{
                if(chunkholder.getTickingChunk() == null){
                    // ControlCraft.LOGGER.info("chunk of holder {} {} is null", p_183756_.x, p_183756_.z);
                }
            }

        }
    }

    @Inject(
            method = "move(Lnet/minecraft/server/level/ServerPlayer;)V",
            at = @At(
                    value = "INVOKE",
                    target = "Lnet/minecraft/core/SectionPos;x()I",
                    shift = At.Shift.BEFORE
            ),
            cancellable = true
    )
    private void moveCamera(ServerPlayer player, CallbackInfo ci) {
        if(!ServerCameraManager.isRegistered(player.getUUID()))return;


        CameraBlockEntity cam = Optional
                .ofNullable(ServerCameraManager.getCamera(player))
                .flatMap(p -> BlockEntityGetter.INSTANCE
                        .getBlockEntityAt(p.globalPos(), CameraBlockEntity.class)
                )
                .orElse(null);

        if(cam == null)return;
        ci.cancel();

        Vec3 camPos = toMinecraft(cam.getCameraPosition());
        SectionPos thisSectionPos = SectionPos.of(camPos);
        SectionPos lastSectionPos = cam.tracker.lastSectionPos();

        this.distanceManager.addPlayer(lastSectionPos, player);


        cam.tracker.setLastSectionPos(thisSectionPos);


        SectionPos copy = Optional
                .of(player)
                .filter(IServerPlayerDuck.class::isInstance)
                .map(IServerPlayerDuck.class::cast)
                .map(s -> s.controlcraft$getAndSetLastSectionPos(SectionPos.of(player.blockPosition())))
                .orElse(player.getLastSectionPos());

        int p_nx = SectionPos.blockToSectionCoord(player.getBlockX());
        int p_nz = SectionPos.blockToSectionCoord(player.getBlockZ());
        int p_ox = copy.getX();
        int p_oz = copy.getZ();

        int c_nx = thisSectionPos.getX();
        int c_nz = thisSectionPos.getZ();
        int c_ox = lastSectionPos.x();
        int c_oz = lastSectionPos.z();
        int viewDistance = this.viewDistance + 1;

        if(thisSectionPos.equals(lastSectionPos) && (p_nx == p_ox && p_nz == p_oz))return;


        Set<Pair<Integer, Integer>> toUnloadSet = new HashSet<>();
        Set<Pair<Integer, Integer>> toLoadSet = new HashSet<>();
        Set<Pair<Integer, Integer>> toMaintain = new HashSet<>();

        util(
                toUnloadSet,
                toLoadSet,
                toMaintain,
                c_nx, c_nz,
                c_ox, c_oz,
                viewDistance
        );

        util(
                toUnloadSet,
                toLoadSet,
                toMaintain,
                p_nx, p_nz,
                p_ox, p_oz,
                viewDistance
        );

        var unload = toUnloadSet.stream().filter(t -> !toMaintain.contains(t)).toList();
        var load = toLoadSet.stream().filter(t -> !toMaintain.contains(t)).toList();

        unload.forEach(
                xz -> updateChunkTracking(
                        player,
                        new ChunkPos(xz.getFirst(), xz.getSecond()),
                        new MutableObject<>(),
                        true,
                        false

                )
        );

        load.forEach(
                xz -> updateChunkTracking(
                        player,
                        new ChunkPos(xz.getFirst(), xz.getSecond()),
                        new MutableObject<>(),
                        false,
                        true

                )
        );

        // ControlCraft.LOGGER.info("moved: {}, {} size: u {}, l {}", thisSectionPos.getX(), thisSectionPos.getZ(), unload, load);


    }

    @Inject(method = "playerLoadedChunk", at = @At("HEAD"))
    private void controlcraft$onPlayerLoadChunk(ServerPlayer p_183761_, MutableObject<ClientboundLevelChunkWithLightPacket> p_183762_, LevelChunk chunkPos, CallbackInfo ci){
        // ControlCraft.LOGGER.info("playerLoadedChunk: {}, {}", chunkPos.getPos().x, chunkPos.getPos().z);
    }

    @Inject(method = "getPlayers", at = @At(value = "INVOKE", target = "Lnet/minecraft/server/level/ServerPlayer;getLastSectionPos()Lnet/minecraft/core/SectionPos;"), locals = LocalCapture.CAPTURE_FAILSOFT)
    private void controlcraft$sendChunksToCameras(
            ChunkPos pos,
            boolean boundaryOnly,
            CallbackInfoReturnable<List<ServerPlayer>> cir,
            Set<ServerPlayer> $,
            ImmutableList.Builder<ServerPlayer> playerList,
            Iterator<Player> $$,
            ServerPlayer player
    ) {
        SectionPos playerPos = player.getLastSectionPos();

        if (!_isChunkInRange(pos.x, pos.z, playerPos.x(), playerPos.z(), viewDistance)) {
            SectionPos camPos = ServerCameraManager.getCameraOrPlayerSection(player);
            if(!_isChunkInRange(pos.x, pos.z, camPos.x(), camPos.z(), viewDistance))return;
            playerList.add(player);
        }
    }

    private void util(
            Set<Pair<Integer, Integer>> toUnloadSet,
            Set<Pair<Integer, Integer>> toLoadSet,
            Set<Pair<Integer, Integer>> toMaintain,
            int nx, int nz,
            int ox, int oz,
            int viewDistance
    ){
        if (Math.abs(ox - nx) <= viewDistance * 2 && Math.abs(oz - nz) <= viewDistance * 2) {
            int xMin = Math.min(nx, ox) - viewDistance;
            int zMin = Math.min(nz, oz) - viewDistance;
            int xMax = Math.max(nx, ox) + viewDistance;
            int zMax = Math.max(nz, oz) + viewDistance;

            for(int x = xMin; x <= xMax; ++x) {
                for(int z = zMin; z <= zMax; ++z) {
                    boolean loaded = _isChunkInRange(x, z, ox, oz, this.viewDistance);
                    boolean toLoad = _isChunkInRange(x, z, nx, nz, this.viewDistance);

                    // if(toMaintain.contains(new Pair<>(x, z)))continue;

                    if(loaded && !toLoad)toUnloadSet.add(new Pair<>(x, z));
                    if(!loaded && toLoad)toLoadSet.add(new Pair<>(x, z));
                    if(loaded && toLoad)toMaintain.add(new Pair<>(x, z));
                }
            }
        } else {
            // teleport
            for(int x = ox - viewDistance; x <= ox + viewDistance; ++x) {
                for(int z = oz - viewDistance; z <= oz + viewDistance; ++z) {
                    if (_isChunkInRange(x, z, ox, oz, this.viewDistance)) {

                        // if(toMaintain.contains(new Pair<>(x, z)))continue;
                        toUnloadSet.add(new Pair<>(x, z));

                    }
                }
            }

            for(int x = nx - viewDistance; x <= nx + viewDistance; ++x) {
                for(int z = nz - viewDistance; z <= nz + viewDistance; ++z) {
                    if (_isChunkInRange(x, z, nx, nz, this.viewDistance)) {
                        // if(toMaintain.contains(new Pair<>(x, z)))continue;

                        toLoadSet.add(new Pair<>(x, z));
                    }
                }
            }
        }
    }



}
