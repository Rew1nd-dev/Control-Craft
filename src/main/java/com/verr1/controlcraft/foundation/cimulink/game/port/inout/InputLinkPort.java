package com.verr1.controlcraft.foundation.cimulink.game.port.inout;


import com.verr1.controlcraft.foundation.cimulink.core.components.Component;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.sources.Source;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;

public class InputLinkPort extends BlockLinkPort{

    private final Component.Port inputPort = new Component.Port();

    public InputLinkPort() {
        super(new Source());
    }

    public double peek(){
        return __raw().peekOutput(0);
    }

    public void input(double value){
        // inputPort.update(value);
        ((Source)__raw()).setInput(value);
    }

    public void tick(){
        // if(!inputPort.dirty())return;

        // propagateCombinational(new PropagateContext(), this);
    }


    @Override
    public NamedComponent create() {
        return new Source();
    }
}
