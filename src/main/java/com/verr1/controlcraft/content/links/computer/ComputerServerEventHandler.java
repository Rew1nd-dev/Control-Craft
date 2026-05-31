package com.verr1.controlcraft.content.links.computer;

import com.verr1.controlcraft.content.links.computer.lua.ComputerDelegateHandler;

public class ComputerServerEventHandler extends ComputerDelegateHandler {

    public ComputerServerEventHandler(ComputerBlockEntity delegate) {
        super(delegate);
    }

    public void onPlayerTouch(int x, int y){
        if(delegate.serverLua == null)return;
        delegate.serverLua.onPlayerTouch(x, y);
    }

    public void onPlayerWatch(int x, int y){
        if(delegate.serverLua == null)return;
        delegate.serverLua.onPlayerWatch(x, y);
    }

    public void onServerTick() {
        if(delegate.serverLua == null)return;
        delegate.serverLua.onServerTick();
    }

}