package com.verr1.controlcraft.foundation.camera;

import com.mojang.authlib.GameProfile;
import com.verr1.controlcraft.content.blocks.camera.CameraBlockEntity;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.common.util.FakePlayer;
import org.joml.Vector3d;

import java.util.UUID;

public class CameraBoundFakePlayer extends FakePlayer {

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

}
