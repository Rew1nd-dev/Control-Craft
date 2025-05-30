package cimulink.v2.components;

import cimulink.v2.Circuit;
import cimulink.v2.Component;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class NamedComponent {



    private final Component unnamed;



    private final Map<String, Integer> namedInputs; // name -> input array index
    private final Map<String, Integer> namedOutputs;

    private String name = "unnamed";

    private final List<String> inputs;
    private final List<String> outputs;

    public NamedComponent(
            Component unnamed,
            List<String> inputs,
            List<String> outputs
    ) {
        this.unnamed = unnamed;
        this.inputs = inputs;
        this.outputs = outputs;

        this.namedInputs = new HashMap<>();
        this.namedOutputs = new HashMap<>();
        for (int i = 0; i < inputs.size(); i++) {
            String name = inputs.get(i);
            namedInputs.put(name, i);
        }
        for (int i = 0; i < outputs.size(); i++) {
            String name = outputs.get(i);
            namedOutputs.put(name, i);
        }
    }

    public NamedComponent withName(String name){
        this.name = name;
        return this;
    }

    public String name(){
        return name;
    }

    public List<String> inputs() {
        return inputs;
    }

    public List<String> outputs() {
        return outputs;
    }

    public Component unnamed() {
        return unnamed;
    }

    public Map<String, Integer> namedInputs() {
        return namedInputs;
    }

    public Map<String, Integer> namedOutputs() {
        return namedOutputs;
    }



    public int outputId(String name){
        return namedOutputs.getOrDefault(name, -1);
    }

    public int inputId(String name){
        return namedInputs.getOrDefault(name, -1);
    }

    public static NamedComponent combinational(Component raw){
        return null;
    }

}
