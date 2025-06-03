package com.verr1.controlcraft.foundation.cimulink.game.port.inout;


import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.cimulink.core.components.Component;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.data.links.BlockPort;

import java.util.HashSet;
import java.util.Set;

public class InputLinkPort{


    private static final Set<BlockPort> EMPTY = new HashSet<>();

    private BlockPort linkedPortInput;

    private final Component.Port inputPort = new Component.Port();

    public void linkTo(BlockPort port){
        linkedPortInput = port;
    }

    public void input(double value){
        inputPort.update(value);
    }

    private void propagateCombinational(){
        if(!inputPort.dirty())return;
        if(linkedPortInput == null)return;
        BlockLinkPort.of(linkedPortInput.pos()).ifPresent(blp -> {

            if(!blp.nameInputs().containsKey(linkedPortInput.portName()))return;

            blp.input(linkedPortInput.portName(), inputPort.retrieve());
            BlockLinkPort.propagateCombinational(new BlockLinkPort.PropagateContext(), blp);
        });

    }

    public void validate(){
        if(linkedPortInput == null)return;
        BlockLinkPort
            .of(linkedPortInput.pos())
            .filter(blp -> blp.nameInputs().containsKey(linkedPortInput.portName()))
            .ifPresentOrElse(
                    $ -> {},
                    () -> {
                        ControlCraft.LOGGER.info("removed invalid blp: " + linkedPortInput);
                        linkedPortInput = null;
                    }
            );
    }

    public void tick(){
        validate();
        propagateCombinational();
    }

}
