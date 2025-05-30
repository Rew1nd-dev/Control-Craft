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




    public Component input(int index, double value){
        inputs.get(index).update(value);
        return this;
    }

    public double output(int index){
        return outputs.get(index).peek();
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
