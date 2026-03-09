package com.verr1.controlcraft.content.links.screen_base.lua;

import com.verr1.controlcraft.content.links.screen_base.ComputerBaseBlockEntity;

public class ComputerDelegateHandler {
    protected final ComputerBaseBlockEntity delegate;

    public ComputerDelegateHandler(ComputerBaseBlockEntity delegate) {
        this.delegate = delegate;
    }
}
