package com.verr1.controlcraft.foundation.cimulink.game.peripheral;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

public class MutablePlant extends NamedComponent {


    private final List<BiConsumer<MutablePlant, Double>> inputHandlers;
    private final List<Function<MutablePlant, Double>> outputHandlers;

    protected MutablePlant(builder initContext) {
        super(initContext.inputs, initContext.outputs);
        inputHandlers = List.copyOf(initContext.inputHandlers);
        outputHandlers = List.copyOf(initContext.outputHandlers);
    }

    @Override
    public List<Integer> propagateTo(int inputIndex) {
        return List.of();
    }

    @Override
    public void onInputChange(Integer... inputIndexes) {

    }

    protected void prePositiveEdge(){

    }

    protected void postPositiveEdge(){

    }

    @Override
    public void onPositiveEdge() {
        try{
            prePositiveEdge();
            changedInput().forEach(i -> inputHandlers.get(i).accept(this, retrieveInput(i)));
            updateOutput(outputHandlers.stream().map(h -> h.apply(this)).toList());
            postPositiveEdge();
        }catch (RuntimeException e){
            ControlCraft.LOGGER.info("error during MutablePlant onPositiveEdge: self class: {}, {}", getClass(), e.getMessage());
            throw e;
        }

    }

    public static class builder{
        List<String> inputs = new ArrayList<>();
        List<BiConsumer<MutablePlant, Double>> inputHandlers = new ArrayList<>();
        List<String> outputs = new ArrayList<>();
        List<Function<MutablePlant, Double>> outputHandlers = new ArrayList<>();

        public builder in(String name, BiConsumer<MutablePlant, Double> inputHandle){
            inputs.add(name);
            inputHandlers.add(inputHandle);
            return this;
        }

        public builder out(String name, Function<MutablePlant, Double> outputHandle){
            outputs.add(name);
            outputHandlers.add(outputHandle);
            return this;
        }
    }
}
