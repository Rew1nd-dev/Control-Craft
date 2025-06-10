package com.verr1.controlcraft.foundation.cimulink.game.port.inout;

import com.verr1.controlcraft.foundation.cimulink.core.components.Component;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.sources.Sink;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;

public class OutputLinkPort extends BlockLinkPort{


    private final Component.Port outputPort = new Component.Port();

    public OutputLinkPort() {
        super(new Sink());
    }


    public double peek(){
        return outputPort.peek();
    }

    public void tick(){
        outputPort.update(__raw().peekInput(0));
    }

    @Override
    public NamedComponent create() {
        return new Sink();
    }


}
