package com.verr1.controlcraft.content.links.screen_base;

import com.verr1.controlcraft.content.links.screen_base.lua.ComputerDelegateHandler;

public class ComputerClientEventHandler extends ComputerDelegateHandler {

    public ComputerClientEventHandler(ComputerBaseBlockEntity delegate) {
        super(delegate);
    }

    public void onClientTick() {
        if(delegate.clientLua == null)return;
        delegate.clientLua.onClientTick();
    }

}
