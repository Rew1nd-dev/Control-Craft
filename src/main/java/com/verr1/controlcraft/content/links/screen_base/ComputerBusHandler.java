package com.verr1.controlcraft.content.links.screen_base;

import com.verr1.controlcraft.content.links.screen_base.lua.ComputerDelegateHandler;
import com.verr1.controlcraft.foundation.cimulink.core.api.IBusAccess;

public class ComputerBusHandler extends ComputerDelegateHandler implements IBusAccess {

    public ComputerBusHandler(ComputerBaseBlockEntity delegate) {
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
