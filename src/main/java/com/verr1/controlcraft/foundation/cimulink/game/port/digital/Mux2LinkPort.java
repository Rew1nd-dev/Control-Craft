package com.verr1.controlcraft.foundation.cimulink.game.port.digital;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.da.Multiplexer;
import com.verr1.controlcraft.foundation.cimulink.core.registry.Factory;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.ICompilable;
import com.verr1.controlcraft.foundation.cimulink.core.registry.CimulinkFactory;

public class Mux2LinkPort extends BlockLinkPort implements ICompilable<Multiplexer> {


    public Mux2LinkPort() {
        super(new Multiplexer(1));
    }

    @Override
    public NamedComponent create() {
        return new Multiplexer(1);
    }

    @Override
    public Multiplexer component() {
        return (Multiplexer)__raw() ;
    }

    @Override
    public Factory<Multiplexer> factory() {
        return CimulinkFactory.MUX;
    }
}
