package com.verr1.controlcraft.foundation.cimulink.game.port.digital;

import com.verr1.controlcraft.foundation.cimulink.game.port.ISummarizable;
import com.verr1.controlcraft.foundation.cimulink.game.port.InspectableLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.SwitchableLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.FFTypes;
import com.verr1.controlcraft.foundation.cimulink.game.registry.CimulinkFactory;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.Summary;

public class FFLinkPort extends InspectableLinkPort<FFTypes> implements ISummarizable {

    public FFLinkPort() {
        super(FFTypes.D_FF);
    }

    @Override
    protected Class<FFTypes> clazz() {
        return FFTypes.class;
    }


    @Override
    public Summary summary() {
        return switch (getCurrentType())
        {
            case D_FF -> CimulinkFactory.D_FF.summarize(__raw());
            case T_FF -> CimulinkFactory.T_FF.summarize(__raw());
            case RS_FF -> CimulinkFactory.RS_FF.summarize(__raw());
            case JK_FF -> CimulinkFactory.JK_FF.summarize(__raw());
            case ASYNC_T_FF -> CimulinkFactory.ASYNC_T_FF.summarize(__raw());
            case ASYNC_D_FF -> CimulinkFactory.ASYNC_D_FF.summarize(__raw());
            case ASYNC_JK_FF -> CimulinkFactory.ASYNC_JK_FF.summarize(__raw());
            case ASYNC_RS_FF -> CimulinkFactory.ASYNC_RS_FF.summarize(__raw());
        };
    }
}
