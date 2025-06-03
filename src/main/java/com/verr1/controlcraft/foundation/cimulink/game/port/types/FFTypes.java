package com.verr1.controlcraft.foundation.cimulink.game.port.types;

import com.verr1.controlcraft.foundation.cimulink.core.components.general.Temporal;
import com.verr1.controlcraft.foundation.cimulink.game.ComponentInstances;

public enum FFTypes implements Inspectable<Temporal<Boolean>>{
    T_FF(ComponentInstances.T_FF),
    D_FF(ComponentInstances.D_FF),
    JK_FF(ComponentInstances.JK_FF),
    RS_FF(ComponentInstances.RS_FF);

    private final ComponentInstances.Inspector<Temporal<Boolean>> inspector;

    FFTypes(ComponentInstances.Inspector<Temporal<Boolean>> inspector) {
        this.inspector = inspector;
    }

    @Override
    public ComponentInstances.Inspector<Temporal<Boolean>> inspector() {
        return inspector;
    }
}
