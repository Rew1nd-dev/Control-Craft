package com.verr1.controlcraft.content.links.computer;

import com.verr1.controlcraft.content.links.computer.lua.ComputerDelegateHandler;
import com.verr1.controlcraft.foundation.cimulink.core.api.IBusAccess;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.port.bus.IBusContext;
import com.verr1.controlcraft.utils.ConstraintClusterBusResolver;
import org.jetbrains.annotations.NotNull;

import java.util.Set;

public class ComputerBusHandler extends ComputerDelegateHandler implements IBusAccess, IBusContext {

    public ComputerBusHandler(ComputerBlockEntity delegate) {
        super(delegate);
    }

    @Override
    public double retrieve(String componentName, String port) {
        return ConstraintClusterBusResolver.retrieve(delegate.getShipOrGroundID(), componentName, port);
    }

    @Override
    public void propagate(String componentName, String port, double value) {
        ConstraintClusterBusResolver.propagate(delegate.getShipOrGroundID(), componentName, port, value);
    }

    @Override
    public @NotNull Set<NamedComponent> access(String name) {
        return ConstraintClusterBusResolver.access(delegate.getShipOrGroundID(), name);
    }

    @Override
    public @NotNull Set<String> allNames() {
        return ConstraintClusterBusResolver.allNames(delegate.getShipOrGroundID());
    }
}