package com.verr1.controlcraft.foundation.cimulink.game.port;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.Inspectable;

import java.util.function.Function;

public abstract class InspectableLinkPort<T extends Enum<?> & Inspectable<? extends NamedComponent>> extends SwitchableLinkPort<T>{
    protected InspectableLinkPort(T defaultValue) {
        super(defaultValue, v -> v.inspector().get());
    }

    @Override
    protected abstract Class<T> clazz();
}
