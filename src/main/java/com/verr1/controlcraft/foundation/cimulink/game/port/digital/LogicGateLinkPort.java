package com.verr1.controlcraft.foundation.cimulink.game.port.digital;


import com.verr1.controlcraft.foundation.cimulink.game.port.SwitchableLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.GateTypes;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;

public class LogicGateLinkPort extends SwitchableLinkPort<GateTypes> {
    public LogicGateLinkPort(WorldBlockPos portPos) {
        super(portPos, GateTypes.AND);
    }

    @Override
    protected Class<GateTypes> clazz() {
        return GateTypes.class;
    }
}
