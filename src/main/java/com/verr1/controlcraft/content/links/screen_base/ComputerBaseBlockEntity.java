package com.verr1.controlcraft.content.links.screen_base;

import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.content.links.screen_base.lua.*;
import com.verr1.controlcraft.content.links.screen_base.lua.render.RenderCmd;
import com.verr1.controlcraft.foundation.cimulink.core.api.IPhysAccess;
import com.verr1.controlcraft.foundation.cimulink.core.api.IWorldAccess;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
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
import java.util.List;
import java.util.Map;

public class ComputerBaseBlockEntity extends OnShipBlockEntity
        implements IComputerServerContext, IComputerClientContext {
    public static final NetworkKey DATA_SYNC = NetworkKey.create("data_sync");
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
                    Object obj = LuaNbtSerializer.deserialize(tag.getCompound(k));
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
    ComputerServerLua serverLua;
    ComputerClientLua clientLua;
    ComputerScreen screen;

    private String code = "";

    public ComputerBaseBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        buildRegistry(DATA_SYNC).withBasic(SerializePort.of(
                this::collectData,
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
        if (level.isClientSide) {
            serverLua = null;
            clientLua = ComputerClientLua.fromCode(this, code);
        } else {
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
        handler().request(true, CODE);
    }

    public Map<String, Object> collectData() {
        Map<String, Object> delta = networkHandler.collectData();
        if (delta == null) {
            return Map.of();
        }
        return delta;
    }

    public void tickPatchData() {
        if (networkHandler.isAnyDirty()) {
            syncForNear(true, DATA_SYNC);
        }
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

    @Override
    public int width() {
        return 0;
    }

    @Override
    public int height() {
        return 0;
    }

}
