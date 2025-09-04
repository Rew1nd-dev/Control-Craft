package com.verr1.controlcraft.unstable.blocks;

import com.mojang.authlib.GameProfile;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.foundation.managers.ServerCameraManager;
import com.verr1.controlcraft.unstable.AIServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.common.util.FakePlayer;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.core.apigame.world.IPlayer;
import org.valkyrienskies.core.apigame.world.PlayerState;
import org.valkyrienskies.mod.common.VSGameUtilsKt;
import org.valkyrienskies.mod.common.util.MinecraftPlayer;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toJOML;
import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public class AiBoundFakePlayer extends FakePlayer {

    public static final Set<AiBoundFakePlayer> INSTANCES = new HashSet<>();

    private boolean isValid = false;
    private final int live = 30;
    private long ownerId = -1L;
    private int liveCounter = 30;

    public AiBoundFakePlayer(ServerLevel level, long ownerId) {
        super(level, new GameProfile(UUID.randomUUID(), "AiBoundFakePlayer"));
        this.ownerId = ownerId;
    }

    public boolean isInPool(){
        return AIServer.MANAGER.isInPool(ownerId);
    }

    public AiBoundFakePlayer(ServerShip ship) {
        this(level(ship), ship.getId());
    }

    public static ServerLevel level(ServerShip ship){
        var key = VSGameUtilsKt.getResourceKey(ship.getChunkClaimDimension());
        ServerLevel level = ControlCraftServer.INSTANCE.getLevel(key);
        return level == null ? ControlCraftServer.OVERWORLD : level;
    }

    public void reset(){
        isValid = true;
        unsetRemoved();
    }

    private ServerLevel getLevel(){
        return Optional.of(serverLevel()).orElse(null);
    }

    public void activate(){
        liveCounter = live;
        if(!getLevel().players().contains(this)){
            addToLevel();
        }
    }

    public void addToLevel(){
        reset();
        INSTANCES.add(this);
        getLevel().addFreshEntity(this);
    }

    public boolean valid(){
        return isValid;
    }

    public void dump(){
        INSTANCES.remove(this);
        isValid = false;
        remove(RemovalReason.DISCARDED);
    }

    protected @Nullable ServerShip getShip(){
        return AIServer.MANAGER.getShipOf(ownerId).orElse(null);
    }

    @Override
    public void tick(){
        if(liveCounter < -1)return;
        if(liveCounter-- < 0)dump();
        ServerShip ship = getShip();
        if(ship == null)return;

        if(getLevel().players().contains(this)){
            Vector3dc p = ship.getTransform().getPositionInWorld();
            moveTo(p.x(), p.y(), p.z());
            getLevel().getChunkSource().move(this);
        }
    }


    public static Set<IPlayer> getAllWatchers(){
        return INSTANCES.stream().map(AiBoundFakePlayer::toMinecraftPlayer).collect(Collectors.toSet());
    }

    public MinecraftPlayer toMinecraftPlayer(){
        return new MinecraftPlayer(this);
    }
}
