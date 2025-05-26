package com.verr1.controlcraft.content.cctweaked.peripheral;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.blocks.transmitter.PeripheralProxyBlockEntity;
import dan200.computercraft.api.lua.*;
import dan200.computercraft.api.peripheral.IComputerAccess;
import dan200.computercraft.api.peripheral.IPeripheral;
import org.jetbrains.annotations.Nullable;

import java.util.concurrent.ExecutionException;

public class TransmitterPeripheral extends AbstractAttachedPeripheral<PeripheralProxyBlockEntity> {


    public TransmitterPeripheral(PeripheralProxyBlockEntity peripheralProxyBlockEntity) {
        super(peripheralProxyBlockEntity);
    }


    @Override
    public boolean equals(@Nullable IPeripheral iPeripheral) {
        if (!(iPeripheral instanceof TransmitterPeripheral p))return false;
        return getTarget().getBlockPos() == p.getTarget().getBlockPos();
    }


    @Override
    public String getType() {
        return "transmitter";
    }


    @LuaFunction
    public final MethodResult callRemote(IComputerAccess access, ILuaContext context, IArguments arguments) throws LuaException {
        String remoteName = arguments.getString(0);
        String methodName = arguments.getString(1);
        try {
            return getTarget().callRemote(access, context, remoteName, methodName, arguments.drop(2));
        } catch (ExecutionException e) {
            ControlCraft.LOGGER.error("Error while calling remote method", e);
        }
        return MethodResult.of("Error while calling remote method");
    }

    @LuaFunction
    public final MethodResult callRemoteAsync(IComputerAccess access, ILuaContext context, IArguments arguments) throws LuaException {
        String slotName = arguments.getString(0);
        String remoteName = arguments.getString(1);
        String methodName = arguments.getString(2);
        try {
            return getTarget().callRemoteAsync(access, context, slotName, remoteName, methodName, arguments.drop(3));
        }catch (ExecutionException e) {
            ControlCraft.LOGGER.error("Error while calling remote method", e);
        }
        return MethodResult.of("Error while calling remote method");
    }

    @LuaFunction
    public void setProtocol(long p){
        getTarget().setProtocol(p);
    }

}
