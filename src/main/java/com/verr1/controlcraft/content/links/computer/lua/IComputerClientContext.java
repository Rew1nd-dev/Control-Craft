package com.verr1.controlcraft.content.links.computer.lua;

import com.verr1.controlcraft.content.links.computer.ComputerScreen;
import com.verr1.controlcraft.content.links.computer.ComputerNetworkHandler;
import com.verr1.controlcraft.content.links.computer.ComputerAuthoritySnapshot;

public interface IComputerClientContext extends IComputerCommonContext {

    ComputerScreen getScreen();

    ComputerNetworkHandler getNetworkHandler();

    boolean canClientSendNetwork();

    boolean requestClientNetworkAuthority();

    boolean releaseClientNetworkAuthority();

    long currentClientNetworkAuthorityToken();

    ComputerAuthoritySnapshot getClientNetworkAuthoritySnapshot();

    long getClientNetworkAuthorityRequestCooldownTicks();

    long getComputerGameTick();

}
