package com.verr1.controlcraft.foundation.cimulink.game.port.digital;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.da.Multiplexer;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;

public class Mux2LinkPort extends BlockLinkPort {


    public Mux2LinkPort() {
        super(new Multiplexer(1));
    }

    @Override
    public NamedComponent create() {
        return new Multiplexer(1);
    }
}
