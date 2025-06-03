package com.verr1.controlcraft.foundation.cimulink.core.components.circuit;



import com.verr1.controlcraft.foundation.cimulink.core.components.Component;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.PlaceHolder;
import com.verr1.controlcraft.foundation.cimulink.core.records.ComponentPort;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Circuit extends NamedComponent {


    List<Component> components;

    List<ComponentPort> wireId2outputComponentPort;
    List<List<ComponentPort>> wireId2inputComponentPorts;

    List<Integer> ordinal2wireId;

    List<List<Integer>> propagateMap;



    // Contract: The First And Second Component Should Be Circuit Input And Output Component,
    // But Initializing Helper Needs "this", So Put A PlaceHolder At Builder
    Circuit(
            List<String> inputNames, List<String> outputNames,
            int inputId, int outputId,
            List<Component> components,
            List<ComponentPort> wireId2outputComponentPort,
            List<List<ComponentPort>> wireId2inputComponentPorts,
            List<Integer> ordinal2wireId
    ) {
        super(inputNames, outputNames);
        this.components = new ArrayList<>(components);
        this.components.set(inputId, new CircuitInputComponent(inputNames));
        this.components.set(outputId, new CircuitOutputComponent(outputNames));
        // this.component2outputWireIds = component2outputWireIds;
        this.wireId2outputComponentPort = wireId2outputComponentPort;
        this.wireId2inputComponentPorts = wireId2inputComponentPorts;
        this.ordinal2wireId = ordinal2wireId;

        this.propagateMap = new ArrayList<>();

        computePropagateMap();



    }

    private void computePropagateMap(){
        for(int i = 0; i < n(); i++){
            input(i, 0.0);
            onInputChange(i);
            this.propagateMap.add(changedOutput());
            retrieveOutput();
        }
    }

    @Override
    public List<Integer> propagateTo(int inputIndex) {
        return propagateMap.get(inputIndex);
    }

    @Override
    public void onInputChange(Integer... indexes) {
        propagate();
    }

    private void propagate(){
        for(int wid: ordinal2wireId){
            if(!needPropagate(wid))continue;
            retrieveAndPropagate(wid);
        }
    }

    private boolean needPropagate(int wireId){
        ComponentPort cp = wireId2outputComponentPort.get(wireId);
        return components.get(cp.componentId()).outputChanged(cp.portId());
    }


    private void retrieveAndPropagate(int wireId){
        ComponentPort cpo = wireId2outputComponentPort.get(wireId);
        double value = components.get(cpo.componentId()).retrieveOutput(cpo.portId());
        Map<Integer, ArrayList<Integer>> affected = split(wireId2inputComponentPorts.get(wireId));
        for(var cp: affected.keySet()){
            List<Integer> affectedPort = affected.get(cp);
            affectedPort.forEach(
                portId -> components.get(cp).input(portId, value)
            );
            components.get(cp).onInputChange(affectedPort.toArray(new Integer[0]));
        }
    }

    private static Map<Integer, ArrayList<Integer>> split(List<ComponentPort> cps){
        Map<Integer, ArrayList<Integer>> result = new HashMap<>();
        cps.forEach(cp ->
            result
                .computeIfAbsent(cp.componentId(), $ -> new ArrayList<>())
                .add(cp.portId())
        );
        return result;
    }

    @Override
    public void reset() {
        components.forEach(Component::reset);
        // reset temporal components
        onPositiveEdge();
    }

    @Override
    public void onPositiveEdge() {
        components.forEach(Component::onPositiveEdge);
        propagate();
    }

    public void cycle(){
        onInputChange(0);
        onPositiveEdge();
    }

    private class CircuitInputComponent extends PlaceHolder {
        public CircuitInputComponent(List<String> inputs) {
            super(List.of(), inputs);
        }

        @Override
        public String name() {
            return Circuit.this.name();
        }

        @Override
        public boolean outputChanged(int index) {
            return Circuit.this.inputChanged(index);
        }

        @Override
        public boolean anyInputChanged() {
            // starts propagating
            return Circuit.this.anyInputChanged();
        }

        @Override
        public List<Integer> changedOutput() {
            return Circuit.this.changedInput();
        }

        @Override
        public List<Double> retrieveOutput() {
            return Circuit.this.retrieveInput();
        }

        @Override
        public double retrieveOutput(int index) {
            return Circuit.this.retrieveInput(index);
        }
    }

    private class CircuitOutputComponent extends PlaceHolder {
        public CircuitOutputComponent(List<String> outputs) {
            super(outputs, List.of());
        }

        @Override
        public String name() {
            return Circuit.this.name();
        }

        @Override
        public Component input(int index, double value) {
            Circuit.this.updateOutput(index, value);
            return this;
        }

        @Override
        public boolean anyOutputChanged() {
            // Stops Propagation
            return false;
        }
    }



    static class NamedInput extends PlaceHolder{
        public NamedInput(List<String> outputs) {
            super(List.of(), outputs);
        }
    }

    static class NamedOutput extends PlaceHolder{
        public NamedOutput(List<String> inputs) {
            super(inputs, List.of());
        }
    }


}
