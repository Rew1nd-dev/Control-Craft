package com.verr1.controlcraft.content.links.computer;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.content.links.computer.lua.*;
import com.verr1.controlcraft.foundation.cimulink.core.api.IPhysAccess;
import com.verr1.controlcraft.foundation.cimulink.core.api.IWorldAccess;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.foundation.network.packets.specific.ComputerSyncPacket;
import com.verr1.controlcraft.registry.ControlCraftPackets;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.fml.DistExecutor;
import org.jetbrains.annotations.Nullable;
import org.luaj.vm2.LuaError;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class ComputerBlockEntity extends OnShipBlockEntity
        implements IComputerServerContext, IComputerClientContext {
    public static final NetworkKey DATA_INITIAL_SYNC = NetworkKey.create("data_sync");
    public static final NetworkKey CODE = NetworkKey.create("code");

    public static final Serializer<Map<String, Object>> LUA_OBJ_MAP_SER = SerializeUtils.of(
            map -> {
                CompoundTag tag = new CompoundTag();
                for (var e : map.entrySet()) {
                    tag.put(e.getKey(), LuaNbtSerializer.serialize(e.getValue()));
                }

                if (tag.sizeInBytes() > 1024) {
                    return new CompoundTag();
                }

                return tag;
            },
            tag -> {
                HashMap<String, Object> map = new HashMap<>();
                tag.getAllKeys().forEach(k -> {
                    Object obj = LuaNbtSerializer.deserialize(tag.get(k));
                    if (obj == null)
                        return;
                    map.put(k, obj);
                });
                return map;
            });

    private final ComputerNetworkHandler networkHandler = new ComputerNetworkHandler();
    private final ComputerServerEventHandler serverEventHandler = new ComputerServerEventHandler(this);
    private final ComputerClientEventHandler clientEventHandler = new ComputerClientEventHandler(this);
    private final ComputerBusHandler computerBus = new ComputerBusHandler(this);
    private final ConcurrentHashMap<String, Double> luaValueMap = new ConcurrentHashMap<>();
    ComputerServerLua serverLua;
    ComputerClientLua clientLua;
    ComputerScreen screen;

    private String code = "";

    public ComputerBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        buildRegistry(DATA_INITIAL_SYNC).withBasic(SerializePort.of(
                () -> collectData(true),
                this::receiveData,
                LUA_OBJ_MAP_SER
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

        DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> () -> screen = new ComputerScreen(this));
    }

    private void loadLua(String code) {
        if (level == null)
            return;
        String partOfCode = code.substring(0, Math.min(code.length(), 100));
        if (level.isClientSide) {
            ControlCraft.LOGGER.info("Loaded With Lua Code On Client: {}", partOfCode);
            serverLua = null;
            clientLua = ComputerClientLua.fromCode(this, code);
        } else {
            ControlCraft.LOGGER.info("Loaded With Lua Code On Server: {}", partOfCode);
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

    public void loadCode(String code) {
        this.code = code;
        loadLua(code);
        syncForNear(true, CODE);
    }

    @Override
    public void initializeClient() {
        super.initializeClient();
        handler().request(true, CODE, DATA_INITIAL_SYNC);
    }

    @Override
    public void initializeServer() {
        super.initializeServer();
        linkStorage().ifPresent(storage -> storage.putComputer(getWorldBlockPos(), this));
    }

    @Override
    public void removeServer() {
        super.removeServer();
        luaValueMap.clear();
        linkStorage().ifPresent(storage -> storage.removeComputer(getWorldBlockPos()));
    }

    public void sendPatchedUpdate(boolean fullUpdate){
        if(level == null)return;
        CompoundTag tag = LUA_OBJ_MAP_SER.serialize(collectData(fullUpdate));
        ControlCraftPackets.sendToNear(level, getBlockPos(), 64, new ComputerSyncPacket(getBlockPos(), tag));
    }

    public Map<String, Object> collectData(boolean fullUpdate) {
        Map<String, Object> delta = fullUpdate ? networkHandler.allData() : networkHandler.collectData();
        if (delta == null) {
            return Map.of();
        }
        return delta;
    }

    public void tickPatchData() {
        if (networkHandler.isAnyDirty()) {
            sendPatchedUpdate(false);
        }
    }

    public void receivePatchedData(CompoundTag nbt) {
        receiveData(LUA_OBJ_MAP_SER.deserialize(nbt));
    }

    public void receiveData(Map<String, Object> delta) {
        for (var e : delta.entrySet()) {
            networkHandler.set(e.getKey(), e.getValue());
        }
    }

    @Override
    public void tickServer() {
        super.tickServer();
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

}
