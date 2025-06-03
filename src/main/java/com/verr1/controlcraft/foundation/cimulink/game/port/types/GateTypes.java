package com.verr1.controlcraft.foundation.cimulink.game.port.types;

import com.verr1.controlcraft.foundation.cimulink.core.components.digital.BooleanCombinational;
import com.verr1.controlcraft.foundation.cimulink.game.ComponentInstances;

public enum GateTypes implements Inspectable<BooleanCombinational> {
    AND(ComponentInstances.AND2),
    OR(ComponentInstances.OR2),
    XOR(ComponentInstances.XOR2),
    NOT(ComponentInstances.NOT),;

    private final ComponentInstances.Inspector<BooleanCombinational> inspector;


    GateTypes(ComponentInstances.Inspector<BooleanCombinational> inspector) {
        this.inspector = inspector;
    }

    @Override
    public ComponentInstances.Inspector<BooleanCombinational> inspector() {
        return inspector;
    }
}
