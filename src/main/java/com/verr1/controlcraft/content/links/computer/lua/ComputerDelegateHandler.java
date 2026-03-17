package com.verr1.controlcraft.content.links.computer.lua;

import com.verr1.controlcraft.content.links.computer.ComputerBlockEntity;

public class ComputerDelegateHandler {
    protected final ComputerBlockEntity delegate;

    public ComputerDelegateHandler(ComputerBlockEntity delegate) {
        this.delegate = delegate;
    }
}
