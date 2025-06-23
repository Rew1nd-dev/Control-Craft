package com.verr1.controlcraft.foundation.cimulink.game.peripheral;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class Plant extends NamedComponent {

    private final List<Consumer<Double>> inputHandlers;
    private final List<Supplier<Double>> outputHandlers;

    protected Plant(builder initContext) {
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

    @Override
    public void onPositiveEdge() {
        changedInput().forEach(i -> inputHandlers.get(i).accept(retrieveInput(i)));
        updateOutput(outputHandlers.stream().map(Supplier::get).toList());
    }



    public static class builder{
        List<String> inputs = new ArrayList<>();
        List<Consumer<Double>> inputHandlers = new ArrayList<>();
        List<String> outputs = new ArrayList<>();
        List<Supplier<Double>> outputHandlers = new ArrayList<>();

        public builder in(String name, Consumer<Double> inputHandle){
            inputs.add(name);
            inputHandlers.add(inputHandle);
            return this;
        }

        public builder out(String name, Supplier<Double> outputHandle){
            outputs.add(name);
            outputHandlers.add(outputHandle);
            return this;
        }
    }

}
