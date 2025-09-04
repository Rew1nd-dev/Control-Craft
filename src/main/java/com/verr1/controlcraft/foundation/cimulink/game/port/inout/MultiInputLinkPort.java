package com.verr1.controlcraft.foundation.cimulink.game.port.inout;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.sources.MultiIO;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;

public class MultiInputLinkPort extends BlockLinkPort {


    public MultiInputLinkPort() {
        super(new MultiIO(8));
    }

    public void setToCircuit(int index, double val){
        if(index < 0 || index >= 8)return;
        ((MultiIO) __raw()).setToCircuit(index, val);
    }

    public double getFromCircuit(int index){
        if(index < 0 || index >= 8)return 0;
        return ((MultiIO) __raw()).getFromCircuit(index);
    }

    @Override
    public NamedComponent create() {
        return new MultiIO(8);
    }
}
