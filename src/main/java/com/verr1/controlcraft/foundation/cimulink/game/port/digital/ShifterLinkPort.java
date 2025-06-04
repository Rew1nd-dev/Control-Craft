package com.verr1.controlcraft.foundation.cimulink.game.port.digital;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.analog.Shifter;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;

public class ShifterLinkPort extends BlockLinkPort {

    private int parallel = 1;
    private int delay = 1;

    public ShifterLinkPort(WorldBlockPos portPos) {
        super(portPos, new Shifter(1, 1));
    }


    public void setParallel(int p){
        parallel = p;
        recreate();
    }

    public void setDelay(int d){
        delay = d;
        recreate();
    }

    @Override
    public NamedComponent create() {
        return new Shifter(delay, parallel);
    }
}
