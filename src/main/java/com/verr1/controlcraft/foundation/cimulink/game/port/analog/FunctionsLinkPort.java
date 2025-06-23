package com.verr1.controlcraft.foundation.cimulink.game.port.analog;

import com.verr1.controlcraft.foundation.cimulink.game.circuit.Summary;
import com.verr1.controlcraft.foundation.cimulink.game.port.ISummarizable;
import com.verr1.controlcraft.foundation.cimulink.game.port.InspectableLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.SwitchableLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.AnalogTypes;
import com.verr1.controlcraft.foundation.cimulink.game.registry.CimulinkFactory;

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
        return switch (getCurrentType()){
            case MIN -> CimulinkFactory.MIN.summarize(__raw());
            case MAX -> CimulinkFactory.MAX.summarize(__raw());
            case PRODUCT -> CimulinkFactory.PRODUCT.summarize(__raw());
            case DIV -> CimulinkFactory.DIV.summarize(__raw());
            case ABS -> CimulinkFactory.ABS.summarize(__raw());
            case ANGLE_FIX -> CimulinkFactory.ANGLE_FIX.summarize(__raw());
            case POWER -> CimulinkFactory.POWER.summarize(__raw());
            case SIN -> CimulinkFactory.SIN.summarize(__raw());
            case COS -> CimulinkFactory.COS.summarize(__raw());
            case TAN -> CimulinkFactory.TAN.summarize(__raw());
        };
    }
}
