package com.verr1.controlcraft.content.links.computer.lua;

import com.verr1.controlcraft.content.links.computer.ComputerBusHandler;
import com.verr1.controlcraft.content.links.computer.ComputerNetworkHandler;
import com.verr1.controlcraft.content.links.computer.ComputerAuthoritySnapshot;
import org.luaj.vm2.LuaError;

import java.util.UUID;

public interface IComputerServerContext extends IComputerCommonContext {

    void onError(LuaError error);

    ComputerBusHandler getCimulinkBus();

    ComputerNetworkHandler getNetworkHandler();

    boolean setForcedNetworkAuthority(UUID playerId);

    boolean clearForcedNetworkAuthority();

    ComputerAuthoritySnapshot getServerNetworkAuthoritySnapshot();

    long getShipOrGroundID();

    long getComputerGameTick();
}
