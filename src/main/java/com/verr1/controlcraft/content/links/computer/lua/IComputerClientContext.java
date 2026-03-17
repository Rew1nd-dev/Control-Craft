package com.verr1.controlcraft.content.links.computer.lua;

import com.verr1.controlcraft.content.links.computer.ComputerScreen;
import com.verr1.controlcraft.content.links.computer.ComputerNetworkHandler;

public interface IComputerClientContext extends IComputerCommonContext {

    ComputerScreen getScreen();

    ComputerNetworkHandler getNetworkHandler();

}
