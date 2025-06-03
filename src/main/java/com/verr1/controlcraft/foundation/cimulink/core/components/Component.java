package com.verr1.controlcraft.foundation.cimulink.core.components;

import java.util.List;
import java.util.stream.IntStream;

public abstract class Component {

    private final List<Port> inputs;
    private final List<Port> outputs;



    public Component(int n, int m) {
        this.inputs = IntStream.range(0, n).mapToObj(name -> new Port()).toList();
        this.outputs = IntStream.range(0, m).mapToObj(name -> new Port()).toList();

    }

    public abstract List<Integer> propagateTo(int inputIndex);

    public abstract void onInputChange(Integer... inputIndexes);

    public abstract void onPositiveEdge();


    public List<Integer> changedOutput(){
        return IntStream.range(0, outputs.size()).filter(i -> outputs.get(i).dirty()).boxed().toList();
    }


    public List<Double> retrieveOutput(){
        return outputs.stream().mapToDouble(Port::retrieve).boxed().toList();
    }

    public List<Double> peekOutput(){
        return outputs.stream().mapToDouble(Port::peek).boxed().toList();
    }


    public double peekOutput(int index){
        return outputs.get(index).peek();
    }

    public double peekInput(int index){
        return inputs.get(index).peek();
    }

    public List<Integer> changedInput(){
        return IntStream.range(0, inputs.size()).filter(i -> inputs.get(i).dirty()).boxed().toList();
    }

    public boolean anyInputChanged(){
        return inputs.stream().anyMatch(Port::dirty);
    }

    public boolean outputChanged(int index){
        return outputs.get(index).dirty();
    }

    public boolean inputChanged(int index){
        return inputs.get(index).dirty();
    }

    public boolean anyOutputChanged(){
        return outputs.stream().anyMatch(Port::dirty);
    }

    protected List<Double> retrieveInput(){
        return inputs.stream().mapToDouble(Port::retrieve).boxed().toList();
    }

    protected  List<Double> peekInput(){
        return inputs.stream().mapToDouble(Port::peek).boxed().toList();
    }

    protected void updateOutput(List<Double> outputValues){
        if(outputValues.size() != outputs.size()){
            throw new IllegalArgumentException("update values size mismatch! expect: " + m() + " got: " + outputValues.size());
        }
        for(int i = 0; i < outputValues.size(); i++){
            updateOutput(i, outputValues.get(i));
        }
    }

    protected void updateOutput(int index, double value){
        outputs.get(index).update(value);
    }

    public Component input(int index, double value){
        inputs.get(index).update(value);
        return this;
    }

    public double output(int index){
        return outputs.get(index).peek();
    }

    public double retrieveOutput(int index){
        return outputs.get(index).retrieve();
    }

    public double retrieveInput(int index){
        return inputs.get(index).retrieve();
    }

    public int n(){
        return inputs.size();
    }

    public int m(){
        return outputs.size();
    }

    public void reset(){}

    public static class Port{

        private double cachedValue;
        private boolean dirty;

        public double retrieve() {
            dirty = false;
            return cachedValue;
        }

        public void update(double value) {
            cachedValue = value;
            dirty = true;
        }

        public boolean dirty() {
            return dirty;
        }

        public double peek() {
            return cachedValue;
        }
    }
}
