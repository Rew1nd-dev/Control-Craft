package cimulink.v2;

import java.util.HashMap;
import java.util.List;
import java.util.stream.IntStream;

public abstract class Component {

    private final List<Port> inputs;
    private final List<Port> outputs;



    /*
    private final List<String> inputNames;
    private final List<String> outputNames;

    private final HashMap<String, Integer> inputIndexMap;
    private final HashMap<String, Integer> outputIndexMap;

    public List<String> inputNames() {
        return inputNames;
    }

    public List<String> outputNames() {
        return outputNames;
    }

    public int outIndex(String name){
        if(!outputIndexMap.containsKey(name)) {
            throw new IllegalArgumentException("Output name '" + name + "' does not exist.");
        }
        return outputIndexMap.get(name);
    }
    public Component input(String name, double value){
        inputs.get(inIndex(name)).update(value);
        return this;
    }

    public double output(String name){
        return outputs.get(outIndex(name)).peek();
    }

    public int inIndex(String name){
        if(!inputIndexMap.containsKey(name)) {
            throw new IllegalArgumentException("Input name '" + name + "' does not exist.");
        }
        return inputIndexMap.get(name);
    }
    *
    * */

    public Component(int n, int m) {
        this.inputs = IntStream.range(0, n).mapToObj(name -> new Port()).toList();
        this.outputs = IntStream.range(0, m).mapToObj(name -> new Port()).toList();

        /*
        this.inputNames = inputNames;
                this.outputNames = outputNames;
                this.inputIndexMap = new HashMap<>();
                for (int i = 0; i < inputNames.size(); i++) {
                    inputIndexMap.put(inputNames.get(i), i);
                }

                this.outputIndexMap = new HashMap<>();
                for (int i = 0; i < outputNames.size(); i++) {
                    outputIndexMap.put(outputNames.get(i), i);
                }
        * */
    }



    public abstract void onInputChange();

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

    protected  List<Integer> changedInput(){
        return IntStream.range(0, inputs.size()).filter(i -> outputs.get(i).dirty()).boxed().toList();
    }

    public boolean anyInputChanged(){
        return inputs.stream().anyMatch(Port::dirty);
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


    public int n(){
        return inputs.size();
    }

    public int m(){
        return outputs.size();
    }




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
