package com.verr1.controlcraft.foundation.cimulink.game.port.digital;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.da.Multiplexer;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;

public class Mux2LinkPort extends BlockLinkPort {


    public Mux2LinkPort(WorldBlockPos portPos) {
        super(portPos, new Multiplexer(1));
    }

    @Override
    public NamedComponent create() {
        return new Multiplexer(1);
    }
}
