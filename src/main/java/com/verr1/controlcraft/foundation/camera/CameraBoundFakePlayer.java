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
    private final ServerLevel level;
    private final int live = 30;
    private int liveCounter = 10;
    private final CameraBlockEntity owner;

    public CameraBoundFakePlayer(ServerLevel level, CameraBlockEntity owner) {
        super(level, new GameProfile(UUID.randomUUID(), "CameraBoundFakePlayer"));
        this.owner = owner;
        this.level = level;
    }

    public void reset(){
        isValid = true;
        unsetRemoved();
    }

    public void activate(ServerPlayer user){
        liveCounter = live;

        if(!level.players().contains(this)){
            addToLevel(user);
        }
    }

    public void addToLevel(ServerPlayer user){
        reset();
        level.addFreshEntity(this);
        owner.tracker.setLastSectionPos(user.getLastSectionPos());
    }

    public boolean valid(){
        return isValid;
    }

    public void dump(){
        isValid = false;
        remove(RemovalReason.DISCARDED);
    }

    public void _tick(){
        if(liveCounter < -1)return;
        if(liveCounter-- < 0)dump();
        if(level.players().contains(this)){
            Vector3d p = owner.getCameraPosition();
            moveTo(p.x, p.y, p.z);
            level.getChunkSource().move(this);
        }
    }

}
