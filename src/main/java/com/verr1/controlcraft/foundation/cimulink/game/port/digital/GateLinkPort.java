package com.verr1.controlcraft.foundation.cimulink.game.port.digital;


import com.verr1.controlcraft.foundation.cimulink.core.components.digital.gates.Gates;
import com.verr1.controlcraft.foundation.cimulink.core.registry.Factory;
import com.verr1.controlcraft.foundation.cimulink.game.port.ICompilable;
import com.verr1.controlcraft.foundation.cimulink.game.port.InspectableLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.GateTypes;
import com.verr1.controlcraft.foundation.cimulink.core.registry.CimulinkFactory;

public class GateLinkPort extends InspectableLinkPort<GateTypes> implements ICompilable<Gates.Gate> {
    public GateLinkPort() {
        super(GateTypes.AND);
    }

    @Override
    protected Class<GateTypes> clazz() {
        return GateTypes.class;
    }

    @Override
    public Gates.Gate component() {
        return (Gates.Gate)__raw();
    }

    @Override
    public Factory<Gates.Gate> factory() {
        return switch (getCurrentType()){
            case AND -> CimulinkFactory.AND_N;
            case OR -> CimulinkFactory.OR_N;
            case XOR -> CimulinkFactory.XOR_N;
            case NOT -> CimulinkFactory.NOT_N;
        };
    }
}
