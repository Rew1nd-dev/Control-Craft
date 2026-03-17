package com.verr1.controlcraft.content.links.computer;

import com.verr1.controlcraft.content.links.computer.lua.ComputerDelegateHandler;

public class ComputerClientEventHandler extends ComputerDelegateHandler {

    public ComputerClientEventHandler(ComputerBlockEntity delegate) {
        super(delegate);
    }

    public void onClientTick() {
        if(delegate.clientLua == null)return;
        delegate.clientLua.onClientTick();
    }

}
