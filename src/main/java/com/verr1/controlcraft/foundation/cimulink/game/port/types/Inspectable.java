package com.verr1.controlcraft.foundation.cimulink.game.port.types;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.ComponentInstances;

public interface Inspectable<T extends NamedComponent> {

    ComponentInstances.Inspector<T> inspector();
}
