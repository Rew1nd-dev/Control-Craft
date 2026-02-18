package com.verr1.controlcraft.foundation.camera;

import com.mojang.authlib.GameProfile;
import com.verr1.controlcraft.content.blocks.camera.CameraBlockEntity;
import com.verr1.controlcraft.foundation.managers.ServerCameraManager;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.common.util.FakePlayer;
import org.jetbrains.annotations.NotNull;
import org.joml.Vector3d;
import org.valkyrienskies.core.apigame.world.IPlayer;
import org.valkyrienskies.core.apigame.world.PlayerState;
import org.valkyrienskies.mod.common.util.MinecraftPlayer;

import java.util.HashMap;
import java.util.UUID;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toJOML;
import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public class CameraBoundFakePlayer extends FakePlayer implements IPlayer {

    private boolean isValid = false;
    private final int live = 30;
    private int liveCounter = 10;
    private final CameraBlockEntity owner;

    public CameraBoundFakePlayer(ServerLevel level, CameraBlockEntity owner) {
        super(level, new GameProfile(UUID.randomUUID(), "CameraBoundFakePlayer"));
        this.owner = owner;
    }

    public void reset(){
        isValid = true;
        unsetRemoved();
    }

    private ServerLevel getLevel(){
        if(!(owner.getLevel() instanceof ServerLevel level)){
            throw new IllegalStateException("CameraBoundFakePlayer must be used in a ServerLevel context");
        }
        return level;
    }

    public void activate(ServerPlayer user){
        liveCounter = live;
        ServerCameraManager.updateCachedCameraPosition(user, toMinecraft(owner.getCameraPosition()));
        if(!getLevel().players().contains(this)){
            addToLevel(user);
        }
    }

    @Override
    public boolean isSpectator() {
        return false;
    }

    public void addToLevel(ServerPlayer user){
        reset();
        getLevel().addFreshEntity(this);
        owner.tracker.setLastSectionPos(user.getLastSectionPos());
    }

    public boolean valid(){
        return isValid;
    }

    public void dump(){
        isValid = false;
        remove(RemovalReason.DISCARDED);
    }


    @Override
    public void tick(){
        if(liveCounter < -1)return;
        if(liveCounter-- < 0)dump();
        if(getLevel().players().contains(this)){
            Vector3d p = owner.getCameraPosition();
            moveTo(p.x, p.y, p.z);
            getLevel().getChunkSource().move(this);
        }
    }

    @NotNull
    @Override
    public String getDimension() {
        return owner.getDimensionID();
    }

    @NotNull
    @Override
    public Vector3d getPosition(@NotNull Vector3d dest) {
        Vector3d camPos = owner.getCameraPosition();
        return dest.set(camPos);
    }



    @NotNull
    @Override
    public PlayerState getPlayerState() {
        return new PlayerState(
                owner.getCameraPosition(),
                owner.readSelf().velocity(),
                owner.getDimensionID(),
                owner.getShipOrGroundID(),
                toJOML(owner.getBlockPos().getCenter())
        );
    }

    @NotNull
    @Override
    public UUID getUuid() {
        return getUUID();
    }

    @Override
    public boolean isAdmin() {
        return false;
    }

    public MinecraftPlayer toMinecraftPlayer(){
        return new MinecraftPlayer(this);
    }

}
