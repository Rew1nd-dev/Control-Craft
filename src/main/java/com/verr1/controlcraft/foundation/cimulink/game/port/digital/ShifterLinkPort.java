package com.verr1.controlcraft.foundation.cimulink.game.port.digital;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.analog.Shifter;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;

public class ShifterLinkPort extends BlockLinkPort {



    private int parallel = 1;
    private int delay = 0;

    public ShifterLinkPort(WorldBlockPos portPos) {
        super(portPos, new Shifter(0, 1));
    }


    public void setParallel(long p){
        parallel = (int)p;
        recreate();
    }

    public void setDelay(long d){
        delay = (int)d;
        recreate();
    }

    public long parallel() {
        return parallel;
    }

    public long delay() {
        return delay;
    }

    @Override
    public NamedComponent create() {
        return new Shifter(delay, parallel);
    }
}
