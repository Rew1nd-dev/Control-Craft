package com.verr1.controlcraft.foundation.cimulink.game.v1;

import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.port.ILinkableBlock;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.data.links.BlockPort;

import java.util.*;

// for connection recording, forming a graph
public abstract class BlockLinkPort__ {
    private static final Set<BlockPort> EMPTY = new HashSet<>();

    private final Map<String, Set<BlockPort>>   forwardLinks  = new HashMap<>();
    private final Map<String, BlockPort>        backwardLinks = new HashMap<>();

    private final WorldBlockPos portPos;

    private final List<String> inputs = new ArrayList<>();
    private final List<String> outputs = new ArrayList<>();
    private final HashMap<String, Integer> nameInputs = new HashMap<>();
    private final HashMap<String, Integer> nameOutputs = new HashMap<>();

    private NamedComponent realTimeComponent;

    protected BlockLinkPort__(WorldBlockPos portPos) {
        this.portPos = portPos;
        recreate();
    }

    public final List<String> inputs(){
        return Collections.unmodifiableList(inputs);
    }

    public final List<String> outputs(){
        return Collections.unmodifiableList(outputs);
    }

    public final Map<String, Integer> nameOutputs() {
        return Collections.unmodifiableMap(nameOutputs);
    }

    public final Map<String, Integer> nameInputs() {
        return Collections.unmodifiableMap(nameInputs);
    }

    private void setInOut(List<String> inputs, List<String> outputs){
        this.inputs.forEach(this::disconnectInput);
        this.outputs.forEach(this::disconnectOutput);
        remapInOut(inputs, outputs);
    }


    public void propagate(String... changedInputs){

    }


    private void remapInOut(List<String> inputs, List<String> outputs){
        this.inputs.clear();
        this.outputs.clear();
        this.nameInputs.clear();
        this.nameOutputs.clear();
        this.inputs.addAll(inputs);
        this.outputs.addAll(outputs);
        for (int i = 0; i < this.inputs.size(); i++){
            nameInputs.put(this.inputs.get(i), i);
        }
        for (int i = 0; i < this.outputs.size(); i++){
            nameOutputs.put(this.outputs.get(i), i);
        }
    }

    public void deleteInput(String name){
        backwardLinks.remove(name);
    }

    public void disconnectInput(String inputPortName){
        if(!backwardLinks.containsKey(inputPortName))return;
        BlockPort linked = backwardLinks.get(inputPortName);
        of(linked.pos()).ifPresent(p -> p.deleteOutput(inputPortName, new BlockPort(pos(), inputPortName)));

        deleteInput(inputPortName);
    }

    public void disconnectOutput(String outputPortName){
        forwardLinks.getOrDefault(outputPortName, EMPTY).forEach(blockPort -> {
            of(blockPort.pos()).ifPresent(p -> p.deleteInput(blockPort.portName()));
        });
        deleteOutput(outputPortName);
    }

    public void disconnectOutput(String outputPortName, BlockPort forwardPort){
        if(!forwardLinks.getOrDefault(outputPortName, EMPTY).contains(forwardPort))return;

        of(forwardPort.pos()).ifPresent(p -> p.deleteInput(forwardPort.portName()));

        deleteOutput(outputPortName);
    }

    public void deleteOutput(String name, BlockPort forwardPort){
        forwardLinks.getOrDefault(name, EMPTY).remove(forwardPort);
        if(forwardLinks.getOrDefault(name, EMPTY).isEmpty())forwardLinks.remove(name);
    }

    public void deleteOutput(String name){
        forwardLinks.remove(name);
    }



    public WorldBlockPos pos(){
        return portPos;
    }


    public void connectTo(String outputPort, WorldBlockPos pos, String inputName){
        BlockLinkPort__ blp = of(pos).orElse(null);
        if(blp == null)return;
        blp.connectBy(pos(), outputPort, inputName);
    }

    public final void connectBy(WorldBlockPos pos, String outputPort, String inputName){
        // connectBy(bp.pos(), bp.portName());
    }

    private static Optional<BlockLinkPort__> of(WorldBlockPos pos){
        /*
        * return BlockEntityGetter.get()
                .getBlockEntityAt(pos, ILinkableBlock.class)
                .map(ILinkableBlock::linkPort);
        * */
        return Optional.empty();
    }


    public abstract NamedComponent create();

    public final void recreate(){
        realTimeComponent = create();
        setInOut(realTimeComponent.inputs(), realTimeComponent.outputs());
    }

}
