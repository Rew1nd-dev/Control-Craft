package com.verr1.controlcraft.content.blocks.transmitter;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.util.concurrent.ListenableFuture;
import com.google.common.util.concurrent.ListenableFutureTask;
import com.simibubi.create.foundation.blockEntity.SmartBlockEntity;
import com.simibubi.create.foundation.blockEntity.behaviour.BlockEntityBehaviour;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.blocks.receiver.PeripheralInterfaceBlockEntity;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.content.cctweaked.peripheral.TransmitterPeripheral;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.managers.PeripheralNetwork;
import dan200.computercraft.api.lua.IArguments;
import dan200.computercraft.api.lua.ILuaContext;
import dan200.computercraft.api.lua.LuaException;
import dan200.computercraft.api.lua.MethodResult;
import dan200.computercraft.api.peripheral.IComputerAccess;
import dan200.computercraft.api.peripheral.IPeripheral;
import dan200.computercraft.shared.Capabilities;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.util.LazyOptional;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

public class PeripheralProxyBlockEntity extends SmartBlockEntity {

    // Example executor for the main thread (replace with your actual executor)

    private long currentProtocol;

    private TransmitterPeripheral peripheral;
    private LazyOptional<IPeripheral> peripheralCap;


    private static final LoadingCache<WorldBlockPos, Optional<PeripheralInterfaceBlockEntity>> cache = CacheBuilder.newBuilder()
        .maximumSize(256)
        .refreshAfterWrite(2, TimeUnit.SECONDS)
        .build(
                new CacheLoader<>() {

                    @Override
                    public @NotNull ListenableFuture<Optional<PeripheralInterfaceBlockEntity>> reload(WorldBlockPos key, Optional<PeripheralInterfaceBlockEntity> oldValue) throws Exception {
                        ListenableFutureTask<Optional<PeripheralInterfaceBlockEntity>> task = ListenableFutureTask.create(() -> load(key));
                        ControlCraftServer.getMainThreadExecutor().execute(task);
                        return task;
                    }

                    @Override
                    public @NotNull Optional<PeripheralInterfaceBlockEntity> load(@NotNull WorldBlockPos pos) throws Exception {
                        return BlockEntityGetter.INSTANCE
                                .getBlockEntityAt(
                                        pos.globalPos(),
                                        PeripheralInterfaceBlockEntity.class
                                );
                    }
                });

    @Override
    public @NotNull <T> LazyOptional<T> getCapability(@NotNull Capability<T> cap, @Nullable Direction side) {
        if(cap == Capabilities.CAPABILITY_PERIPHERAL){
            if(this.peripheral == null){
                this.peripheral = new TransmitterPeripheral(this);
            }
            if(peripheralCap == null || !peripheralCap.isPresent())
                peripheralCap =  LazyOptional.of(() -> this.peripheral);
            return peripheralCap.cast();
        }
        return super.getCapability(cap, side);
    }

    public void setProtocol(long p){
        currentProtocol = p;
    }

    public PeripheralProxyBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
    }

    public MethodResult callRemote(
            IComputerAccess access,
            ILuaContext context,
            String peripheralName,
            String methodName,
            IArguments args) throws LuaException, ExecutionException {

        WorldBlockPos peripheralPos = PeripheralNetwork.valid(new PeripheralNetwork.PeripheralKey(currentProtocol, peripheralName));
        if(peripheralPos == null){
            ControlCraft.LOGGER.error("Peripheral Not Found In Network: {}", peripheralName);
            return MethodResult.of(null, "Receiver Not Registered");
        }
        if(getLevel() == null)return MethodResult.of(null, "Level Is Null");
        PeripheralInterfaceBlockEntity receiver = cache.get(peripheralPos).orElse(null);
        if(receiver == null){
            ControlCraft.LOGGER.error("Receiver is null: {}", peripheralName);
            return MethodResult.of(null, "Peripheral Is Not A Receiver");
        }
        if(receiver.isRemoved()){
            ControlCraft.LOGGER.error("Receiver is already removed!: {}", peripheralName);
        }
        return receiver
                    .callPeripheral(
                            access,
                            context,
                            methodName,
                            args
                    );
    }

    public MethodResult callRemoteAsync(IComputerAccess access,
                                        ILuaContext context,
                                        String slot,
                                        String peripheralName,
                                        String methodName,
                                        IArguments args)
            throws LuaException, ExecutionException {
        WorldBlockPos peripheralPos = PeripheralNetwork.valid(new PeripheralNetwork.PeripheralKey(currentProtocol, peripheralName));
        if(peripheralPos == null)return MethodResult.of(null, "Receiver Not Registered");
        if(getLevel() == null)return MethodResult.of(null, "Level Is Null");
        PeripheralInterfaceBlockEntity receiver = cache.get(peripheralPos).orElse(null);
        if(receiver == null)return MethodResult.of(null, "Peripheral Is Not A Receiver");
        return receiver
                .callPeripheralAsync(
                        access,
                        context,
                        slot,
                        methodName,
                        args
                );
    }

    @Override
    public void tick(){
        if(level.isClientSide)return;
    }

    @Override
    public void invalidate(){
        super.invalidate();
        if(peripheralCap != null){
            peripheralCap.invalidate();
            peripheralCap = null;
        }
    }

    @Override
    public void addBehaviours(List<BlockEntityBehaviour> behaviours) {

    }
}
