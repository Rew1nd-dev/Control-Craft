package com.verr1.controlcraft.foundation.managers;

import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.blocks.camera.CameraBlockEntity;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies;
import net.minecraft.core.BlockPos;
import net.minecraft.core.SectionPos;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.level.ChunkPos;
import net.minecraft.world.phys.Vec3;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.HashMap;
import java.util.Optional;
import java.util.UUID;

public class ServerCameraManager {
    private static final HashMap<UUID, WorldBlockPos> player2Camera = new HashMap<>();
    private static final HashMap<WorldBlockPos, UUID> camera2Player = new HashMap<>();

    private static final UUID RANDOM_UUID = UUID.randomUUID();

    public static void registerUser(WorldBlockPos cameraPos, ServerPlayer player){
        player2Camera.put(player.getUUID(), cameraPos);
        camera2Player.put(cameraPos, player.getUUID());
    }

    public static void remove(WorldBlockPos pos){
        UUID player = camera2Player.get(pos);
        player2Camera.remove(player);
        camera2Player.remove(pos);
    }

    public static boolean isRegistered(WorldBlockPos pos){
        return camera2Player.containsKey(pos);
    }

    public static boolean isRegistered(UUID player){
        return player2Camera.containsKey(player);
    }

    public static void remove(UUID player){
        WorldBlockPos pos = player2Camera.get(player);
        player2Camera.remove(player);
        camera2Player.remove(pos);
    }

    public static @Nullable ServerPlayer getUser(WorldBlockPos pos){
        return Optional.ofNullable(ControlCraftServer.INSTANCE)
                .map(MinecraftServer::getPlayerList)
                .map(list -> list.getPlayer(camera2Player.get(pos)))
                .orElse(null);
    }

    public static @NotNull UUID getUserUUID(WorldBlockPos pos){
        return Optional.ofNullable(ControlCraftServer.INSTANCE)
                .map(MinecraftServer::getPlayerList)
                .map(list -> list.getPlayer(camera2Player.get(pos)))
                .map(Entity::getUUID)
                .orElse(RANDOM_UUID);
    }

    public static @Nullable WorldBlockPos getCamera(ServerPlayer player){
        return player2Camera.get(player.getUUID());
    }

    public static Vec3 getCameraOrPlayerPos(ServerPlayer player){
        return Optional
                .ofNullable(ServerCameraManager.getCamera(player))
                .flatMap(p -> BlockEntityGetter.INSTANCE
                        .getBlockEntityAt(p.globalPos(), CameraBlockEntity.class)
                )
                .map(CameraBlockEntity::getCameraPosition)
                .map(ValkyrienSkies::toMinecraft)
                .orElse(player.position());
    }

    public static @NotNull  ChunkPos getCameraOrPlayerChunkPos(ServerPlayer player){
        return new ChunkPos(BlockPos.containing(getCameraOrPlayerPos(player)));
    }

    public static @NotNull SectionPos getCameraOrPlayerSection(ServerPlayer player){
        return SectionPos.of(BlockPos.containing(getCameraOrPlayerPos(player)));
    }

    public static @Nullable ChunkPos getCameraChunk(ServerPlayer player){
        if(isRegistered(player.getUUID())) return new ChunkPos(player2Camera.get(player.getUUID()).pos());
        return null;
    }



    public static boolean hasNearByCamera(Vec3 position){
        for(WorldBlockPos pos : camera2Player.keySet()){
            if(pos.pos().getCenter().distanceTo(position) < 10 * 16){
                return true;
            }
        }
        return false;
    }



}
