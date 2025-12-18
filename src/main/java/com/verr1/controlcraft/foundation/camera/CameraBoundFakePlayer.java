package com.verr1.controlcraft.foundation.camera;

import com.mojang.authlib.GameProfile;
import com.verr1.controlcraft.content.blocks.camera.CameraBlockEntity;
import com.verr1.controlcraft.foundation.managers.ServerCameraManager;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.common.util.FakePlayer;
import org.jetbrains.annotations.NotNull;
import org.joml.Vector3d;
import org.valkyrienskies.core.internal.world.VsiPlayer;
import org.valkyrienskies.core.internal.world.VsiPlayerState;
import org.valkyrienskies.mod.common.util.MinecraftPlayer;

import java.util.HashMap;
import java.util.Set;
import java.util.UUID;

import static org.valkyrienskies.mod.api.ValkyrienSkies.toJOML;
import static org.valkyrienskies.mod.api.ValkyrienSkies.toMinecraft;


public class CameraBoundFakePlayer extends FakePlayer implements VsiPlayer {

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
    public VsiPlayerState getPlayerState() {
        return new VsiPlayerState(
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

    @Override
    public @NotNull Set<Long> getForceWatchingShips() {
        return Set.of(owner.getShipOrGroundID());
    }
}
