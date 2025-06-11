package com.verr1.controlcraft.foundation.cimulink.game.circuit;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.Circuit;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.CircuitConstructor;
import com.verr1.controlcraft.foundation.cimulink.core.records.ComponentPortName;
import com.verr1.controlcraft.foundation.cimulink.game.registry.CimulinkFactory;

import java.util.List;

public class CircuitNbt {
    List<ComponentNbt> componentSummaries;
    List<ConnectionNbt> connectionNbts;
    List<IoNbt> inOuts;


    CircuitNbt(
            List<ComponentNbt> componentSummaries,
            List<ConnectionNbt> connectionNbts,
            List<IoNbt> inOuts
    ){
        this.componentSummaries = componentSummaries;
        this.connectionNbts = connectionNbts;
        this.inOuts = inOuts;
    }

    public Circuit buildCircuit(){
        List<NamedComponent> components = componentSummaries.stream().map(
                s -> {
                    String name = s.componentName();
                    NamedComponent component = CimulinkFactory.restore(s.componentTag(), NamedComponent.class);
                    component.withName(name);
                    return component;
                }
        ).toList();

        CircuitConstructor constructor = new CircuitConstructor();

        constructor.addComponent(components.toArray(new NamedComponent[0]));

        for (ConnectionNbt connectionNbt : connectionNbts) {
            constructor.connect(
                    connectionNbt.outputName(),
                    connectionNbt.outputPortName(),
                    connectionNbt.inputName(),
                    connectionNbt.inputPortName()
            );
        }

        for (IoNbt io: inOuts){
            if(io.isInput()){
                constructor.defineInput(io.ioName(), io.componentName(), io.portName());
            }else{
                constructor.defineOutput(io.ioName(), io.componentName(), io.portName());
            }
        }

        return constructor.build();

    }

}
