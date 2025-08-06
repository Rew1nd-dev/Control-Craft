package com.verr1.controlcraft.foundation.cimulink.game.port.analog;

import com.verr1.controlcraft.foundation.cimulink.game.circuit.Summary;
import com.verr1.controlcraft.foundation.cimulink.game.port.ISummarizable;
import com.verr1.controlcraft.foundation.cimulink.game.port.InspectableLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.AnalogTypes;

public class FunctionsLinkPort extends InspectableLinkPort<AnalogTypes> implements ISummarizable {
    public FunctionsLinkPort() {
        super(AnalogTypes.MAX);
    }

    @Override
    protected Class<AnalogTypes> clazz() {
        return AnalogTypes.class;
    }


    @Override
    public Summary summary() {
        return __raw().summary();

    }
}
