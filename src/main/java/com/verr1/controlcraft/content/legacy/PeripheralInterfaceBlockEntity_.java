package com.verr1.controlcraft.content.legacy;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.blocks.NetworkBlockEntity;
import com.verr1.controlcraft.content.blocks.receiver.PeripheralInterfaceBlock;
import com.verr1.controlcraft.foundation.api.IPacketHandler;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.CompoundTagPort;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.foundation.network.packets.BlockBoundClientPacket;
import com.verr1.controlcraft.foundation.network.packets.BlockBoundServerPacket;
import com.verr1.controlcraft.foundation.type.RegisteredPacketType;
import com.verr1.controlcraft.registry.ControlCraftPackets;
import com.verr1.controlcraft.utils.SerializeUtils;
import dan200.computercraft.api.lua.IArguments;
import dan200.computercraft.api.lua.ILuaContext;
import dan200.computercraft.api.lua.LuaException;
import dan200.computercraft.api.lua.MethodResult;
import dan200.computercraft.api.peripheral.IComputerAccess;
import dan200.computercraft.api.peripheral.IPeripheral;
import dan200.computercraft.core.methods.PeripheralMethod;
import dan200.computercraft.impl.Peripherals;
import dan200.computercraft.shared.computer.core.ServerContext;
import dan200.computercraft.shared.platform.InvalidateCallback;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.network.NetworkEvent;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

public class PeripheralInterfaceBlockEntity_ extends NetworkBlockEntity implements
        IPacketHandler
{
    public static final NetworkKey PERIPHERAL = NetworkKey.create("peripheral");
    public static final NetworkKey PERIPHERAL_TYPE = NetworkKey.create("peripheral_type");

    private IPeripheral attachedPeripheral;
    private final ConcurrentHashMap<String, PeripheralMethod> methods = new ConcurrentHashMap<>();


    private PeripheralKey_ networkKey = PeripheralKey_.NULL;


    private final ConcurrentHashMap<String, Runnable> syncedTasks = new ConcurrentHashMap<>();

    public synchronized MethodResult callPeripheral(IComputerAccess access, ILuaContext context, String methodName, IArguments args) throws LuaException {
        if(level == null || level.isClientSide)return MethodResult.of(null, "You Are Calling This On The Client Side, Nothing Returned");
        if(attachedPeripheral == null) return MethodResult.of(null, "Receiver Called, But No Peripheral Attached");
        if(!methods.containsKey(methodName))return MethodResult.of(null, "Receiver Called, But Method Not Found");
        if(access == null)return MethodResult.of(null, "Receiver Called, But No Access Provided");

        return Optional.ofNullable(methods.get(methodName)).map(m -> {
            try {
                return m.apply(attachedPeripheral, context, access, args);
            } catch (LuaException e) {
                throw new RuntimeException(e);
            }catch (NullPointerException e){
                String peripheralName = Optional.ofNullable(attachedPeripheral).map(IPeripheral::getType).orElse("null peripheral");
                ControlCraft.LOGGER.error("Peripheral {} Called, But Method {} Not Found", peripheralName, methodName);
            }
            return null;
        }).orElse(MethodResult.of(null, "Exception Occurred"));


    }

    public synchronized MethodResult callPeripheralAsync(IComputerAccess access, ILuaContext context, String slot, String methodName, IArguments args){
        if(level == null || level.isClientSide)return MethodResult.of(null, "You Are Calling This On The Client Side, Nothing Returned");
        if(attachedPeripheral == null) return MethodResult.of(null, "Receiver Called, But No Peripheral Attached");
        if(!methods.containsKey(methodName))return MethodResult.of(null, "Receiver Called, But Method Not Found");
        if(access == null)return MethodResult.of(null, "Receiver Called, But No Access Provided");
        // attachedPeripheral may be set to null after the task being queued
        final IPeripheral snapshot = attachedPeripheral;
        enqueueTask(slot, ()->{
            try {
                methods.get(methodName).apply(snapshot, context, access, args);
            } catch (Exception e) {
                ControlCraft.LOGGER.info("Lua Exception Of: {}", e.getMessage());
            }
        });
        return MethodResult.of("queued");
    }

    public void enqueueTask(String slot, Runnable r){
        if(syncedTasks.size() < 256) syncedTasks.put(slot, r);
    }

    public void executeAll(){
        if(syncedTasks.isEmpty())return;
        var iterator = syncedTasks.entrySet().iterator();
        while(iterator.hasNext()){
            var entry = iterator.next();
            try{
                entry.getValue().run();
            }catch (Exception e){
                ControlCraft.LOGGER.error("Error while executing task: {}", e.getMessage());
            }

            iterator.remove();
        }
    }


    public synchronized String getAttachedPeripheralType(){
        if(level == null || level.isClientSide)return "You Are Calling This On The Client Side, Nothing Returned";
        if(attachedPeripheral == null)return "Not Attached";
        return attachedPeripheral.getType();
    }


    public synchronized void deleteAttachedPeripheral(){
        attachedPeripheral = null;
        methods.clear();
    }

    public synchronized void updateAttachedPeripheral(){
        if(level == null || level.isClientSide)return;
        deleteAttachedPeripheral();
        Direction attachedDirection = getBlockState().getValue(PeripheralInterfaceBlock.FACING);
        BlockPos attachedPos = getBlockPos()
                .offset(
                        attachedDirection
                                .getOpposite()
                                .getNormal()
                );
        IPeripheral peripheral = Peripherals.getPeripheral(
                (ServerLevel)level,
                attachedPos,
                attachedDirection,
                new InvalidPeripheralCallBack()
        );
        attachedPeripheral = peripheral;
        if(attachedPeripheral == null)return;
        methods.clear();
        methods.putAll(ServerContext.get(((ServerLevel) level).getServer()).peripheralMethods().getSelfMethods(peripheral));
    }

    public PeripheralKey_ getNetworkKey(){
        if(registered())return networkKey;
        return PeripheralKey_.NULL;
    }



    public boolean registered(){
        return NetworkManager.isRegistered(getBlockPos());
    }


    public void resetNetworkRegistry(PeripheralKey_ newKey){
        if(level == null){
            ControlCraft.LOGGER.error("trying to register network key when level is null !");
            return;
        }
        if(level.isClientSide) {
            ControlCraft.LOGGER.error("trying to register network key in client side !");
            return;
        }
        if(Objects.equals(newKey.Name(), ""))return;
        networkKey = NetworkManager.registerAndGetKey(newKey, getBlockPos());
        setChanged();
    }

    @Override
    public void tickServer(){
        NetworkManager.activate(getBlockPos());
    }

    @Override
    public void tickCommon() {
        super.tickCommon();
        executeAll();
    }

    @Override
    public void lazyTickServer() {
        updateAttachedPeripheral();
        syncForNear(true, PERIPHERAL, PERIPHERAL_TYPE);
    }

    public PeripheralInterfaceBlockEntity_(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        buildRegistry(PERIPHERAL_TYPE).withBasic(SerializePort.of(
                    () -> Optional.ofNullable(attachedPeripheral).map(IPeripheral::getType).orElse("Not Attached"),
                    $ -> {},
                    SerializeUtils.STRING
                )).withClient(new ClientBuffer<>(SerializeUtils.STRING, String.class)).runtimeOnly().register();



        buildRegistry(PERIPHERAL).withBasic(CompoundTagPort.of(
                () -> getNetworkKey().serialize(),
                tag -> enqueueTask("reset", () -> resetNetworkRegistry(PeripheralKey_.deserialize(tag)))
        )).withClient(new ClientBuffer<>(SerializeUtils.of(PeripheralKey_::serialize, PeripheralKey_::deserialize), PeripheralKey_.class)).register();

    }

    @Override
    public void remove(){
        super.remove();
        if(level == null || level.isClientSide)return;
        NetworkManager.UnregisterWirelessPeripheral(networkKey);
    }



    protected void displayScreen(ServerPlayer player){

        PeripheralKey_ networkKey = getNetworkKey();
        String peripheralType = getAttachedPeripheralType();
        var p = new BlockBoundClientPacket.builder(getBlockPos(), RegisteredPacketType.SETTING_0)
                .withLong(networkKey.Protocol())
                .withUtf8(peripheralType)
                .withUtf8(networkKey.Name())
                .build();

        ControlCraftPackets.sendToPlayer(p, player);

    }


    @Override
    public void handleServer(NetworkEvent.Context context, BlockBoundServerPacket packet) {
        if(packet.getType() == RegisteredPacketType.SETTING_0){
            String name = packet.getUtf8s().get(0);
            Long protocol = packet.getLongs().get(0);
            resetNetworkRegistry(new PeripheralKey_(name, protocol));
        }
    }

    private class InvalidPeripheralCallBack implements InvalidateCallback{
        @Override
        public void run() {
            ControlCraft.LOGGER.info("Peripheral {} Invalidated", Optional.ofNullable(attachedPeripheral).map(IPeripheral::getType).orElse("null"));
            enqueueTask("invalidate", PeripheralInterfaceBlockEntity_.this::deleteAttachedPeripheral);
        }
    }
}
