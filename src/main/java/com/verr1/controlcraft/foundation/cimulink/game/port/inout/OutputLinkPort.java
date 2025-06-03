package com.verr1.controlcraft.foundation.cimulink.game.port.inout;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.cimulink.core.components.Component;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.data.links.BlockPort;

public class OutputLinkPort {

    private BlockPort linkedPortOutput;

    private final Component.Port outputPort = new Component.Port();

    public void linkTo(BlockPort port){
        linkedPortOutput = port;
    }

    public void sample(){
        BlockLinkPort
            .of(linkedPortOutput.pos())
            .ifPresent(blp -> {
                if(!blp.nameOutputs().containsKey(linkedPortOutput.portName()))return;
                outputPort.update(blp.output(linkedPortOutput.portName()));
            });
    }

    public double output(){
        return outputPort.retrieve();
    }

    public void validate(){
        if(linkedPortOutput == null)return;
        BlockLinkPort
            .of(linkedPortOutput.pos())
            .filter(blp -> blp.nameInputs().containsKey(linkedPortOutput.portName()))
            .ifPresentOrElse(
                $ -> {},
                () -> {
                    ControlCraft.LOGGER.info("removed invalid blp: " + linkedPortOutput);
                    linkedPortOutput = null;
                }
            );
    }

    public void tick(){
        validate();
        sample();
    }


}
