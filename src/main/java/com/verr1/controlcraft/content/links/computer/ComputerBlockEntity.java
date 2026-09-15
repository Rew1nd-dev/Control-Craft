package com.verr1.controlcraft.content.links.computer;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.content.blocks.SharedKeys;
import com.verr1.controlcraft.content.links.computer.lua.*;
import com.verr1.controlcraft.foundation.cimulink.core.api.IPhysAccess;
import com.verr1.controlcraft.foundation.cimulink.core.api.IWorldAccess;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.network.executors.CompoundTagPort;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.foundation.network.packets.specific.ComputerAuthorityRequestPacket;
import com.verr1.controlcraft.foundation.network.packets.specific.ComputerAuthorityReleasePacket;
import com.verr1.controlcraft.foundation.network.packets.specific.ComputerAuthoritySyncPacket;
import com.verr1.controlcraft.foundation.network.packets.specific.ComputerSyncPacket;
import com.verr1.controlcraft.registry.ControlCraftPackets;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.fml.DistExecutor;
import org.jetbrains.annotations.Nullable;
import org.luaj.vm2.LuaError;

import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

public class ComputerBlockEntity extends OnShipBlockEntity
        implements IComputerServerContext, IComputerClientContext {
    public static final NetworkKey DATA_INITIAL_SYNC = NetworkKey.create("data_sync");
    public static final NetworkKey CODE = NetworkKey.create("code");
    public static final NetworkKey RELOAD_CODE = NetworkKey.create("reload_code");
    private static final int CLIENT_AUTHORITY_REQUEST_INTERVAL_TICKS = 10;
    private static final int SERVER_AUTHORITY_LEASE_TICKS = 30;
    private static final int SERVER_AUTHORITY_SYNC_INTERVAL_TICKS = 20;

    private final ComputerNetworkHandler networkHandler = new ComputerNetworkHandler();
    private final ComputerServerEventHandler serverEventHandler = new ComputerServerEventHandler(this);
    private final ComputerClientEventHandler clientEventHandler = new ComputerClientEventHandler(this);
    private final ComputerBusHandler computerBus = new ComputerBusHandler(this);
    private final ConcurrentHashMap<String, Double> luaValueMap = new ConcurrentHashMap<>();
    ComputerServerLua serverLua;
    ComputerClientLua clientLua;
    ComputerScreen screen;

    private String deviceName = "";
    private String code = "";
    private @Nullable UUID forcedNetworkAuthorityOwner;
    private @Nullable UUID leasedNetworkAuthorityOwner;
    private long leasedNetworkAuthorityExpireTick = 0L;
    private long serverNetworkAuthorityToken = 0L;
    private long lastAuthoritySyncTick = Long.MIN_VALUE;
    private volatile ComputerAuthoritySnapshot clientNetworkAuthoritySnapshot = ComputerAuthoritySnapshot.none();
    private long nextClientAuthorityRequestTick = Long.MIN_VALUE;

    public ComputerBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        buildRegistry(DATA_INITIAL_SYNC).withBasic(CompoundTagPort.of(
                () -> collectSyncTag(true),
                this::receivePatchedData
            ))
                .dispatchToSync()
                .runtimeOnly()
                .register();

        buildRegistry(CODE).withBasic(SerializePort.of(
                this::code,
                this::loadCode,
                SerializeUtils.STRING
            ))
                .dispatchToSync()
                .register();

        panel().registerUnit(RELOAD_CODE, this::reloadCode);

        DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> () -> screen = new ComputerScreen(this));
    }

    private void loadLua(String code) {
        if (level == null)
            return;
        String partOfCode = code.substring(0, Math.min(code.length(), 100));
        if (level.isClientSide) {
            // ControlCraft.LOGGER.info("Loaded With Lua Code On Client: {}", partOfCode);
            serverLua = null;
            clientLua = ComputerClientLua.fromCode(this, code);
        } else {
            // ControlCraft.LOGGER.info("Loaded With Lua Code On Server: {}", partOfCode);
            serverLua = ComputerServerLua.fromCode(this, code);
            clientLua = null;
        }
    }

    public @Nullable ComputerScreen getScreen() {
        return screen;
    }

    public String code() {
        return code;
    }

    @Override
    public void setDeviceName(String name) {
        this.deviceName = name == null ? "" : name;
        if (level != null && !level.isClientSide) {
            setChanged();
            syncForNear(false, SharedKeys.COMPONENT_NAME);
        }
    }

    @Override
    public String deviceName() {
        return deviceName;
    }

    public void loadCode(String code) {
        this.code = code;
        loadLua(code);
        syncForNear(true, CODE);
    }

    public void reloadCode() {
        if (level == null || level.isClientSide) {
            return;
        }

        try {
            loadCode(code);
        } catch (LuaError error) {
            onError(error);
        } catch (RuntimeException error) {
            ControlCraft.LOGGER.error(
                    "Computer Lua reload failed at {}: {}",
                    getBlockPos().toShortString(),
                    error.getMessage(),
                    error
            );
        }
    }

    @Override
    public void initializeClient() {
        super.initializeClient();
        clientNetworkAuthoritySnapshot = ComputerAuthoritySnapshot.none();
        nextClientAuthorityRequestTick = Long.MIN_VALUE;
        handler().request(true, CODE, DATA_INITIAL_SYNC);
    }

    @Override
    public void initializeServer() {
        super.initializeServer();
        clearServerAuthorityState();
        linkStorage().ifPresent(storage -> storage.putComputer(getWorldBlockPos(), this));
    }

    @Override
    public void removeServer() {
        super.removeServer();
        luaValueMap.clear();
        clearServerAuthorityState();
        linkStorage().ifPresent(storage -> storage.removeComputer(getWorldBlockPos()));
    }

    @Override
    public void removeClient() {
        super.removeClient();
        releaseClientNetworkAuthority();
        clientNetworkAuthoritySnapshot = ComputerAuthoritySnapshot.none();
        nextClientAuthorityRequestTick = Long.MIN_VALUE;
    }

    public void sendPatchedUpdate(boolean fullUpdate){
        if(level == null)return;
        CompoundTag tag = collectSyncTag(fullUpdate);
        ControlCraftPackets.sendToNear(level, getBlockPos(), 64, new ComputerSyncPacket(getBlockPos(), tag));
    }

    public CompoundTag collectSyncTag(boolean fullUpdate) {
        return fullUpdate ? networkHandler.collectFullSyncTag() : networkHandler.collectDeltaSyncTag();
    }

    public void tickPatchData() {
        if (networkHandler.hasPendingSync()) {
            sendPatchedUpdate(false);
        }
    }

    public void receivePatchedData(CompoundTag nbt) {
        networkHandler.applySyncTag(nbt == null ? new CompoundTag() : nbt);
    }

    @Override
    public void tickServer() {
        super.tickServer();
        tickAuthorityState();
        tickPatchData();
        serverEventHandler.onServerTick();
    }

    @Override
    public void tickClient() {
        super.tickClient();
        clientEventHandler.onClientTick();
    }

    @Override
    public void onError(LuaError error) {
        ControlCraft.LOGGER.error(
                "Computer Lua error at {}: {}",
                getBlockPos().toShortString(),
                LuaUtils.sanitizeLuaError(error, 400)
        );
    }

    public ComputerServerEventHandler getServerEventHandler() {
        return serverEventHandler;
    }

    @Override
    public ComputerBusHandler getCimulinkBus() {
        return computerBus;
    }

    @Override
    public ComputerNetworkHandler getNetworkHandler() {
        return networkHandler;
    }

    @Override
    public long getComputerGameTick() {
        return level == null ? 0L : level.getGameTime();
    }

    @Override
    public boolean canClientSendNetwork() {
        UUID playerId = getClientPlayerId();
        if (playerId == null) {
            return false;
        }
        return getEffectiveClientAuthoritySnapshot().isOwnedBy(playerId, getClientGameTick());
    }

    @Override
    public boolean requestClientNetworkAuthority() {
        if (level == null || !level.isClientSide) {
            return false;
        }

        ComputerAuthoritySnapshot snapshot = getEffectiveClientAuthoritySnapshot();
        UUID playerId = getClientPlayerId();
        if (playerId == null) {
            return false;
        }

        if (snapshot.mode() == ComputerAuthorityMode.FORCED) {
            return snapshot.isOwnedBy(playerId, getClientGameTick());
        }

        long gameTick = getClientGameTick();
        if (gameTick < nextClientAuthorityRequestTick) {
            return snapshot.isOwnedBy(playerId, gameTick);
        }

        ControlCraftPackets.getChannel().sendToServer(new ComputerAuthorityRequestPacket(getBlockPos()));
        nextClientAuthorityRequestTick = gameTick + CLIENT_AUTHORITY_REQUEST_INTERVAL_TICKS;
        return true;
    }

    @Override
    public boolean releaseClientNetworkAuthority() {
        if (level == null || !level.isClientSide) {
            return false;
        }

        ComputerAuthoritySnapshot snapshot = getEffectiveClientAuthoritySnapshot();
        UUID playerId = getClientPlayerId();
        if (playerId == null || snapshot.mode() != ComputerAuthorityMode.LEASED || !snapshot.isOwnedBy(playerId, getClientGameTick())) {
            return false;
        }

        ControlCraftPackets.getChannel().sendToServer(new ComputerAuthorityReleasePacket(getBlockPos()));
        clientNetworkAuthoritySnapshot = ComputerAuthoritySnapshot.none();
        nextClientAuthorityRequestTick = Long.MIN_VALUE;
        return true;
    }

    @Override
    public long currentClientNetworkAuthorityToken() {
        return getEffectiveClientAuthoritySnapshot().token();
    }

    @Override
    public ComputerAuthoritySnapshot getClientNetworkAuthoritySnapshot() {
        return getEffectiveClientAuthoritySnapshot();
    }

    @Override
    public long getClientNetworkAuthorityRequestCooldownTicks() {
        long nextRequestTick = nextClientAuthorityRequestTick;
        if (nextRequestTick == Long.MIN_VALUE) {
            return 0L;
        }

        long gameTick = getClientGameTick();
        if (nextRequestTick <= gameTick) {
            return 0L;
        }

        return nextRequestTick - gameTick;
    }

    @Override
    public boolean setForcedNetworkAuthority(UUID playerId) {
        if (playerId == null || level == null || level.isClientSide) {
            return false;
        }

        long gameTick = level.getGameTime();
        ComputerAuthoritySnapshot before = snapshotFromServerState(gameTick);
        boolean leaseCleared = leasedNetworkAuthorityOwner != null || leasedNetworkAuthorityExpireTick != 0L;

        forcedNetworkAuthorityOwner = playerId;
        leasedNetworkAuthorityOwner = null;
        leasedNetworkAuthorityExpireTick = 0L;

        if (before.mode() != ComputerAuthorityMode.FORCED || !playerId.equals(before.ownerId())) {
            advanceServerAuthorityToken();
        }

        ComputerAuthoritySnapshot after = snapshotFromServerState(gameTick);
        if (leaseCleared || !before.equals(after)) {
            syncAuthorityToNearby(after, gameTick);
        }
        return true;
    }

    @Override
    public boolean clearForcedNetworkAuthority() {
        if (level == null || level.isClientSide || forcedNetworkAuthorityOwner == null) {
            return false;
        }

        long gameTick = level.getGameTime();
        ComputerAuthoritySnapshot before = snapshotFromServerState(gameTick);
        forcedNetworkAuthorityOwner = null;
        leasedNetworkAuthorityOwner = null;
        leasedNetworkAuthorityExpireTick = 0L;

        if (before.isActive()) {
            advanceServerAuthorityToken();
        }

        syncAuthorityToNearby(snapshotFromServerState(gameTick), gameTick);
        return true;
    }

    @Override
    public ComputerAuthoritySnapshot getServerNetworkAuthoritySnapshot() {
        if (level == null || level.isClientSide) {
            return ComputerAuthoritySnapshot.none();
        }

        long gameTick = level.getGameTime();
        if (cleanupLeasedAuthority(gameTick)) {
            syncAuthorityToNearby(snapshotFromServerState(gameTick), gameTick);
        }
        return snapshotFromServerState(gameTick);
    }

    public IPhysAccess getPhysAccess() {
        if (level == null || level.isClientSide) {
            return IPhysAccess.ofMainThread(this);
        }
        return IPhysAccess.of(this);
    }

    public IWorldAccess getWorldAccess() {
        return IWorldAccess.ofImmediate(this);
    }

    public void setLuaValue(String key, double value) {
        luaValueMap.put(key, value);
    }

    public boolean hasLuaValue(String key) {
        return luaValueMap.containsKey(key);
    }

    public void clearLuaValue(String key) {
        luaValueMap.remove(key);
    }

    public @Nullable Double getLuaValue(String key) {
        return luaValueMap.get(key);
    }

    public boolean requestNetworkAuthority(ServerPlayer sender) {
        if (level == null || level.isClientSide) {
            return false;
        }

        long gameTick = level.getGameTime();
        if (cleanupLeasedAuthority(gameTick)) {
            syncAuthorityToNearby(snapshotFromServerState(gameTick), gameTick);
        }

        UUID senderId = sender.getUUID();
        ComputerAuthoritySnapshot before = snapshotFromServerState(gameTick);

        if (forcedNetworkAuthorityOwner != null) {
            return forcedNetworkAuthorityOwner.equals(senderId);
        }

        if (leasedNetworkAuthorityOwner == null) {
            leasedNetworkAuthorityOwner = senderId;
            leasedNetworkAuthorityExpireTick = gameTick + SERVER_AUTHORITY_LEASE_TICKS;
            advanceServerAuthorityToken();
        } else if (leasedNetworkAuthorityOwner.equals(senderId)) {
            leasedNetworkAuthorityExpireTick = gameTick + SERVER_AUTHORITY_LEASE_TICKS;
        } else {
            return false;
        }

        ComputerAuthoritySnapshot after = snapshotFromServerState(gameTick);
        if (!before.equals(after)) {
            syncAuthorityToNearby(after, gameTick);
        }
        return true;
    }

    public boolean releaseNetworkAuthority(ServerPlayer sender) {
        if (level == null || level.isClientSide || leasedNetworkAuthorityOwner == null) {
            return false;
        }

        UUID senderId = sender.getUUID();
        if (!senderId.equals(leasedNetworkAuthorityOwner)) {
            return false;
        }

        long gameTick = level.getGameTime();
        ComputerAuthoritySnapshot before = snapshotFromServerState(gameTick);
        leasedNetworkAuthorityOwner = null;
        leasedNetworkAuthorityExpireTick = 0L;

        if (before.mode() == ComputerAuthorityMode.LEASED) {
            advanceServerAuthorityToken();
        }

        syncAuthorityToNearby(snapshotFromServerState(gameTick), gameTick);
        return true;
    }

    public boolean acceptClientNetworkInput(ServerPlayer sender, long authorityToken) {
        if (level == null || level.isClientSide) {
            return false;
        }

        long gameTick = level.getGameTime();
        if (cleanupLeasedAuthority(gameTick)) {
            syncAuthorityToNearby(snapshotFromServerState(gameTick), gameTick);
        }

        ComputerAuthoritySnapshot snapshot = snapshotFromServerState(gameTick);
        return snapshot.token() != 0L
                && snapshot.token() == authorityToken
                && snapshot.isOwnedBy(sender.getUUID(), gameTick);
    }

    public void receiveAuthoritySync(ComputerAuthoritySnapshot snapshot) {
        clientNetworkAuthoritySnapshot = snapshot == null ? ComputerAuthoritySnapshot.none() : snapshot;
    }

    private void tickAuthorityState() {
        if (level == null || level.isClientSide) {
            return;
        }

        long gameTick = level.getGameTime();
        if (cleanupLeasedAuthority(gameTick)) {
            syncAuthorityToNearby(snapshotFromServerState(gameTick), gameTick);
            return;
        }

        ComputerAuthoritySnapshot snapshot = snapshotFromServerState(gameTick);
        if (snapshot.isActive() && gameTick - lastAuthoritySyncTick >= SERVER_AUTHORITY_SYNC_INTERVAL_TICKS) {
            syncAuthorityToNearby(snapshot, gameTick);
        }
    }

    private void syncAuthorityToNearby(ComputerAuthoritySnapshot snapshot, long gameTick) {
        if (level == null || level.isClientSide) {
            return;
        }

        lastAuthoritySyncTick = gameTick;
        ControlCraftPackets.sendToNear(level, getBlockPos(), 64, new ComputerAuthoritySyncPacket(getBlockPos(), snapshot));
    }

    private boolean cleanupLeasedAuthority(long gameTick) {
        if (leasedNetworkAuthorityOwner == null) {
            return false;
        }

        if (forcedNetworkAuthorityOwner != null) {
            leasedNetworkAuthorityOwner = null;
            leasedNetworkAuthorityExpireTick = 0L;
            return false;
        }

        if (gameTick <= leasedNetworkAuthorityExpireTick && isServerPlayerOnline(leasedNetworkAuthorityOwner)) {
            return false;
        }

        leasedNetworkAuthorityOwner = null;
        leasedNetworkAuthorityExpireTick = 0L;
        return true;
    }

    private ComputerAuthoritySnapshot snapshotFromServerState(long gameTick) {
        if (forcedNetworkAuthorityOwner != null) {
            return new ComputerAuthoritySnapshot(ComputerAuthorityMode.FORCED, forcedNetworkAuthorityOwner, 0L, serverNetworkAuthorityToken);
        }
        if (leasedNetworkAuthorityOwner != null && gameTick <= leasedNetworkAuthorityExpireTick) {
            return new ComputerAuthoritySnapshot(
                    ComputerAuthorityMode.LEASED,
                    leasedNetworkAuthorityOwner,
                    leasedNetworkAuthorityExpireTick,
                    serverNetworkAuthorityToken
            );
        }
        return ComputerAuthoritySnapshot.none();
    }

    private ComputerAuthoritySnapshot getEffectiveClientAuthoritySnapshot() {
        ComputerAuthoritySnapshot snapshot = clientNetworkAuthoritySnapshot == null
                ? ComputerAuthoritySnapshot.none()
                : clientNetworkAuthoritySnapshot;

        if (snapshot.mode() == ComputerAuthorityMode.LEASED && getClientGameTick() > snapshot.leaseExpireTick()) {
            return ComputerAuthoritySnapshot.none();
        }
        return snapshot;
    }

    private long advanceServerAuthorityToken() {
        serverNetworkAuthorityToken++;
        if (serverNetworkAuthorityToken == 0L) {
            serverNetworkAuthorityToken = 1L;
        }
        return serverNetworkAuthorityToken;
    }

    private void clearServerAuthorityState() {
        forcedNetworkAuthorityOwner = null;
        leasedNetworkAuthorityOwner = null;
        leasedNetworkAuthorityExpireTick = 0L;
        serverNetworkAuthorityToken = 0L;
        lastAuthoritySyncTick = Long.MIN_VALUE;
    }

    private boolean isServerPlayerOnline(UUID playerId) {
        return level != null
                && !level.isClientSide
                && level.getServer() != null
                && level.getServer().getPlayerList().getPlayer(playerId) != null;
    }

    private @Nullable UUID getClientPlayerId() {
        return Minecraft.getInstance().player == null ? null : Minecraft.getInstance().player.getUUID();
    }

    private long getClientGameTick() {
        return level == null ? 0L : level.getGameTime();
    }

}
