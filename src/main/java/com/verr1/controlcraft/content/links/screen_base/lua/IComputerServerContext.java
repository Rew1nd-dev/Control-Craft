package com.verr1.controlcraft.content.links.screen_base.lua;

import com.verr1.controlcraft.content.links.screen_base.ComputerBusHandler;
import com.verr1.controlcraft.content.links.screen_base.ComputerNetworkHandler;
import org.luaj.vm2.LuaError;

public interface IComputerServerContext extends IComputerCommonContext {

    void onError(LuaError error);

    ComputerBusHandler getCimulinkBus();

    ComputerNetworkHandler getNetworkHandler();
}
