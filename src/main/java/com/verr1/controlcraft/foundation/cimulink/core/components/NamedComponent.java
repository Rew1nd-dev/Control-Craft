package com.verr1.controlcraft.foundation.cimulink.core.components;




import com.verr1.controlcraft.foundation.cimulink.core.records.ComponentPortName;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils.checkName;


public abstract class NamedComponent extends Component {


    private final Map<String, Integer> namedInputs; // name -> input array index
    private final Map<String, Integer> namedOutputs;

    private String name = "unnamed";

    private final List<String> inputs;
    private final List<String> outputs;

    public NamedComponent(
            List<String> inputs,
            List<String> outputs
    ) {
        super(inputs.size(), outputs.size());

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
        ArrayUtils.AssertDifferent(inputs);
        ArrayUtils.AssertDifferent(outputs);
        ArrayUtils.AssertDifferent(namedOutputs.keySet(), namedInputs.keySet());
    }

    public NamedComponent withName(String name){
        checkName(name);
        this.name = name;
        return this;
    }

    public String in(int index){
        ArrayUtils.AssertRange(index, n());
        return inputs().get(index);
    }

    public String out(int index){
        ArrayUtils.AssertRange(index, m());
        return outputs().get(index);
    }

    public NamedComponent input(String name, double value){
        ArrayUtils.AssertExistence(namedInputs.keySet(), name);
        input(namedInputs.get(name), value);
        return this;
    }

    public double output(String name){
        ArrayUtils.AssertExistence(namedOutputs.keySet(), name);
        return output(namedOutputs.get(name));
    }

    public ComponentPortName __in(int index){
        return new ComponentPortName(name(), in(index));
    }

    public ComponentPortName __out(int index){
        return new ComponentPortName(name(), out(index));
    }

    public String in(){
        return in(0);
    }

    public String out(){
        return out(0);
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


    public Map<String, Integer> namedInputs() {
        return namedInputs;
    }

    public Map<String, Integer> namedOutputs() {
        return namedOutputs;
    }


    public List<String> propagateTo(String name) {
        if (!namedInputs.containsKey(name)) {
            throw new IllegalArgumentException("Output name '" + name + "' does not exist in component '" + this.outputs() + "'");
        }
        return propagateTo(namedInputs.get(name)).stream().map(p -> outputs().get(p)).toList();
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
