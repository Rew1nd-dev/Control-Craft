package com.verr1.controlcraft.content.links.computer;

import com.verr1.controlcraft.content.links.computer.lua.ComputerDelegateHandler;
import com.verr1.controlcraft.foundation.cimulink.core.api.IBusAccess;

public class ComputerBusHandler extends ComputerDelegateHandler implements IBusAccess {

    public ComputerBusHandler(ComputerBlockEntity delegate) {
        super(delegate);
    }

    @Override
    public double retrieve(String componentName, String port) {
        return 0;
    }

    @Override
    public void propagate(String componentName, String port, double value) {

    }
}
