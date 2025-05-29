package com.verr1.controlcraft.content.blocks.receiver;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.blocks.NetworkBlockEntity;
import com.verr1.controlcraft.foundation.api.IPacketHandler;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.managers.PeripheralNetwork;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.CompoundTagPort;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
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
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

public class PeripheralInterfaceBlockEntity extends NetworkBlockEntity implements
        IPacketHandler
{
    public static final NetworkKey PERIPHERAL = NetworkKey.create("peripheral");
    public static final NetworkKey VALID_PERIPHERAL = NetworkKey.create("valid_peripheral");
    public static final NetworkKey PERIPHERAL_TYPE = NetworkKey.create("peripheral_type");
    public static final NetworkKey FORCED = NetworkKey.create("forced");
    public static final NetworkKey ONLINE = NetworkKey.create("online");
    public static final NetworkKey OFFLINE = NetworkKey.create("offline");

    private IPeripheral attachedPeripheral;
    private final ConcurrentHashMap<String, PeripheralMethod> methods = new ConcurrentHashMap<>();




    private PeripheralNetwork.PeripheralKey holdKey = PeripheralNetwork.PeripheralKey.NULL;



    private boolean forced = false;


    private final ConcurrentHashMap<String, Runnable> syncedTasks = new ConcurrentHashMap<>();

    public synchronized MethodResult callPeripheral(IComputerAccess access, ILuaContext context, String methodName, IArguments args) throws LuaException {
        if(level == null || level.isClientSide)return MethodResult.of(null, "You Are Calling This On The Client Side, Nothing Returned");
        if(attachedPeripheral == null){
            ControlCraft.LOGGER.error("Peripheral h:{}, v:{} Called, But No Peripheral Attached", holdKey, valid());
            return MethodResult.of(null, "Receiver Called, But No Peripheral Attached");
        }
        if(!methods.containsKey(methodName)){
            ControlCraft.LOGGER.error("Peripheral Called, But Method {} Not Found", methodName);
            return MethodResult.of(null, "Receiver Called, But Method Not Found");
        }
        if(access == null){
            ControlCraft.LOGGER.error("Peripheral Called, But No Access Provided");
            return MethodResult.of(null, "Receiver Called, But No Access Provided");
        }

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
        if(attachedPeripheral == null){
            ControlCraft.LOGGER.error("Async Peripheral h:{}, v:{} Called, But No Peripheral Attached", holdKey, valid());
            return MethodResult.of(null, "Receiver Called, But No Peripheral Attached");
        }
        if(!methods.containsKey(methodName)){
            ControlCraft.LOGGER.error("Async Peripheral Called, But Method {} Not Found", methodName);
            return MethodResult.of(null, "Receiver Called, But Method Not Found");
        }
        if(access == null){
            ControlCraft.LOGGER.error("Async Peripheral Called, But No Access Provided");
            return MethodResult.of(null, "Receiver Called, But No Access Provided");
        }
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
                () -> {}
        );
        attachedPeripheral = peripheral;
        if(attachedPeripheral == null)return;
        methods.clear();
        methods.putAll(ServerContext.get(((ServerLevel) level).getServer()).peripheralMethods().getSelfMethods(peripheral));
    }

    public PeripheralNetwork.PeripheralKey getHoldKey(){
        return holdKey;
    }

    public void setHoldKey(PeripheralNetwork.PeripheralKey holdKey) {
        if(Objects.equals(holdKey.name(), ""))return;

        this.holdKey = holdKey;

        enqueueTask("online", this::onlineAccordingly);

        setChanged();
    }

    private void onlineAccordingly(){

        if(forced)forceOnline();
        else softOnline();
    }

    private void forceOnline(){
        if(level == null || level.isClientSide)return;
        PeripheralNetwork.forceOnline(holdKey, WorldBlockPos.of(level, getBlockPos()));
    }

    private void softOnline(){
        if(level == null || level.isClientSide)return;
        PeripheralNetwork.softOnline(holdKey, WorldBlockPos.of(level, getBlockPos()));
    }

    public void offline(){
        PeripheralNetwork.offline(holdKey);
    }

    public boolean forced() {
        return forced;
    }

    public void setForced(boolean forced) {
        this.forced = forced;
    }

    public PeripheralNetwork.PeripheralKey valid(){
        return PeripheralNetwork.valid(WorldBlockPos.of(level, getBlockPos()));
    }


    public boolean isOnline(){
        return PeripheralNetwork.valid(WorldBlockPos.of(level, getBlockPos())).equals(holdKey);
    }

    @Override
    public void tickServer(){
        PeripheralNetwork.activate(WorldBlockPos.of(level, getBlockPos()));
    }

    @Override
    public void tickCommon() {
        super.tickCommon();
        executeAll();
    }

    @Override
    public void lazyTickServer() {
        updateAttachedPeripheral();

        if(forced && !isOnline())forceOnline();


        syncForNear(true, PERIPHERAL, PERIPHERAL_TYPE, VALID_PERIPHERAL);
    }

    public PeripheralInterfaceBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        buildRegistry(PERIPHERAL_TYPE).withBasic(SerializePort.of(
                    () -> Optional.ofNullable(attachedPeripheral).map(IPeripheral::getType).orElse("Not Attached"),
                    $ -> {},
                    SerializeUtils.STRING
                )).withClient(new ClientBuffer<>(SerializeUtils.STRING, String.class)).runtimeOnly().register();



        buildRegistry(PERIPHERAL).withBasic(CompoundTagPort.of(
                () -> getHoldKey().serialize(),
                tag -> setHoldKey(PeripheralNetwork.PeripheralKey.deserialize(tag))
        )).withClient(
                new ClientBuffer<>(
                        SerializeUtils.of(
                                PeripheralNetwork.PeripheralKey::serialize,
                                PeripheralNetwork.PeripheralKey::deserialize
                        ),
                        PeripheralNetwork.PeripheralKey.class)
        ).register();


        buildRegistry(VALID_PERIPHERAL).withBasic(CompoundTagPort.of(
                () -> valid().serialize(),
                $ -> {}
        )).withClient(
                new ClientBuffer<>(
                        SerializeUtils.of(
                                PeripheralNetwork.PeripheralKey::serialize,
                                PeripheralNetwork.PeripheralKey::deserialize
                        ),
                        PeripheralNetwork.PeripheralKey.class)
        )
                .runtimeOnly()
                .dispatchToSync()
                .register();

        buildRegistry(FORCED)
                .withBasic(SerializePort.of(this::forced, this::setForced, SerializeUtils.BOOLEAN))
                .withClient(ClientBuffer.BOOLEAN.get())
                .register();

        panel().registerUnit(ONLINE, this::onlineAccordingly);
        panel().registerUnit(OFFLINE, () -> {offline(); setForced(false);});

    }

    @Override
    public void remove(){
        super.remove();
        if(level == null || level.isClientSide)return;

        //

    }






    private class InvalidPeripheralCallBack implements InvalidateCallback{
        @Override
        public void run() {
            ControlCraft.LOGGER.info("Peripheral {} Invalidated", Optional.ofNullable(attachedPeripheral).map(IPeripheral::getType).orElse("null"));
            enqueueTask("invalidate", PeripheralInterfaceBlockEntity.this::deleteAttachedPeripheral);
        }
    }
}
