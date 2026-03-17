package com.verr1.controlcraft.content.links.computer.lua;

import com.verr1.controlcraft.content.links.computer.ComputerBusHandler;
import com.verr1.controlcraft.content.links.computer.ComputerNetworkHandler;
import org.luaj.vm2.LuaError;

public interface IComputerServerContext extends IComputerCommonContext {

    void onError(LuaError error);

    ComputerBusHandler getCimulinkBus();

    ComputerNetworkHandler getNetworkHandler();
}
