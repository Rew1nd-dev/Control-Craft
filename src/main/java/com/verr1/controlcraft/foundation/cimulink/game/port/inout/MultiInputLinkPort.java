package com.verr1.controlcraft.foundation.cimulink.game.port.inout;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.sources.MultiSource;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;

public class MultiInputLinkPort extends BlockLinkPort {


    public MultiInputLinkPort() {
        super(new MultiSource(8));
    }

    public void setInput(int index, double val){
        if(index < 0 || index >= 8)return;
        ((MultiSource) __raw()).setInput(index, val);
    }


    @Override
    public NamedComponent create() {
        return new MultiSource(8);
    }
}
