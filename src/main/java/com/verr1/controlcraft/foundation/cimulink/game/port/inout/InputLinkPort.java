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
        return inputPort.peek();
    }

    public void input(double value){
        inputPort.update(value);
    }

    public void tick(){
        if(!inputPort.dirty())return;
        ((Source)__raw()).setInput(inputPort.retrieve());
        propagateCombinational(new PropagateContext(), this);
    }


    @Override
    public NamedComponent create() {
        return new Source();
    }
}
