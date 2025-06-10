package com.verr1.controlcraft.foundation.cimulink.game.port.digital;


import com.verr1.controlcraft.foundation.cimulink.game.port.SwitchableLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.GateTypes;

public class GateLinkPort extends SwitchableLinkPort<GateTypes> {
    public GateLinkPort() {
        super(GateTypes.AND);
    }

    @Override
    protected Class<GateTypes> clazz() {
        return GateTypes.class;
    }
}
