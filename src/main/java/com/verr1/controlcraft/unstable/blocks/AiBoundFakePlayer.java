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
import org.valkyrienskies.core.api.ships.LoadedServerShip;
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.core.api.ships.Ship;
import org.valkyrienskies.core.internal.world.VsiPlayer;
import org.valkyrienskies.core.internal.world.VsiPlayerState;
import org.valkyrienskies.mod.common.VSGameUtilsKt;
import org.valkyrienskies.mod.common.util.MinecraftPlayer;

import java.util.*;
import java.util.stream.Collectors;


public class AiBoundFakePlayer extends FakePlayer implements VsiPlayer {

    public static final Set<AiBoundFakePlayer> INSTANCES = new HashSet<>();

    private boolean isValid = false;
    private final int live = 30;
    private long ownerId = -1L;
    private int liveCounter = 30;
    private UUID uuid = UUID.randomUUID();

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

    protected Optional<LoadedServerShip> getShip(){
        return AIServer.MANAGER.getShipOf(ownerId);
    }

    @Override
    public void tick(){
        if(liveCounter < -1)return;
        if(liveCounter-- < 0)dump();
        LoadedServerShip ship = getShip().orElse(null);
        if(ship == null)return;

        if(getLevel().players().contains(this)){
            Vector3dc p = ship.getTransform().getPositionInWorld();
            moveTo(p.x(), p.y(), p.z());
            getLevel().getChunkSource().move(this);
        }
    }


    public static Set<VsiPlayer> getAllWatchers(){
        return INSTANCES.stream().map(AiBoundFakePlayer::toMinecraftPlayer).collect(Collectors.toSet());
    }

    public MinecraftPlayer toMinecraftPlayer(){
        return new MinecraftPlayer(this);
    }

    @Override
    public @NotNull Vector3d getPosition(@NotNull Vector3d vector3d) {
        return getShip().map(s -> new Vector3d(s.getTransform().getPositionInWorld())).orElse(new Vector3d());
    }

    @Override
    public @NotNull VsiPlayerState getPlayerState() {
        Optional<LoadedServerShip> ship = getShip();
        return new VsiPlayerState(
                ship.map(s -> new Vector3d(s.getTransform().getPositionInWorld())).orElse(new Vector3d()),
                ship.map(Ship::getVelocity).orElse(new Vector3d()),
                getDimension(),
                null,
                null
        );
    }

    @Override
    public @NotNull String getDimension() {
        return getShip().map(Ship::getChunkClaimDimension).orElse(ControlCraftServer.OVERWORLD.dimension().toString());
    }

    @Override
    public @NotNull UUID getUuid() {
        return uuid;
    }

    @Override
    public boolean isAdmin() {
        return false;
    }

    @Override
    public @NotNull Set<Long> getForceWatchingShips() {
        return Set.of(ownerId);
    }
}
