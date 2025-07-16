package com.verr1.controlcraft.foundation.cimulink.core.components;




import com.verr1.controlcraft.foundation.cimulink.core.records.ComponentPortName;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;

import java.util.*;

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

        this.inputs = Collections.unmodifiableList(inputs);
        this.outputs = Collections.unmodifiableList(outputs);

        Map<String, Integer> namedInputs = new HashMap<>();
        Map<String, Integer> namedOutputs = new HashMap<>();
        for (int i = 0; i < inputs.size(); i++) {
            String name = inputs.get(i);
            namedInputs.put(name, i);
        }
        for (int i = 0; i < outputs.size(); i++) {
            String name = outputs.get(i);
            namedOutputs.put(name, i);
        }

        this.namedInputs = Collections.unmodifiableMap(namedInputs);
        this.namedOutputs = Collections.unmodifiableMap(namedOutputs);


        ArrayUtils.AssertDifferent(inputs);
        ArrayUtils.AssertDifferent(outputs);
        ArrayUtils.AssertDifferent(this.namedOutputs.keySet(), this.namedInputs.keySet());


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
        ArrayUtils.AssertPresence(namedInputs.keySet(), name);
        input(namedInputs.get(name), value);
        return this;
    }

    public double output(String name){
        ArrayUtils.AssertPresence(namedOutputs.keySet(), name);
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

    public final List<String> inputsExcludeSignals(){
        return inputs().stream().filter(s -> !s.contains("@")).toList();
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



    public double retrieveOutput(String name) {
        ArrayUtils.AssertPresence(outputs, name);
        return super.retrieveOutput(namedOutputs.get(name));
    }

    public double peekOutput(String name) {
        ArrayUtils.AssertPresence(outputs, name);
        return super.peekOutput(namedOutputs.get(name));
    }

    public double retrieveInput(String name) {
        ArrayUtils.AssertPresence(outputs, name);
        return super.retrieveInput(namedOutputs.get(name));
    }

    public double peekInput(String name) {
        ArrayUtils.AssertPresence(inputs, name);
        return super.peekInput(namedInputs.get(name));
    }

    public List<String> propagateTo(String name) {
        if (!namedInputs.containsKey(name)) {
            throw new IllegalArgumentException("Output name '" + name + "' does not exist in component '" + this.outputs() + "'");
        }
        return propagateTo(namedInputs.get(name)).stream().map(p -> outputs().get(p)).toList();
    }

    public void onInputChange(String... changedInputs){
        for(var name: changedInputs){
            if (namedInputs.containsKey(name))continue;
            throw new IllegalArgumentException("Input name '" + name + "' does not exist in component '" + this.inputs() + "'");
        }

        onInputChange(Arrays.stream(changedInputs).map(namedInputs::get).toList().toArray(new Integer[0]));
    }


    public List<String> changedOutputName() {
        return super.changedOutput().stream().map(outputs::get).toList();
    }

    public List<String> changedInputName() {
        return super.changedInput().stream().map(inputs::get).toList();
    }

    public int outputId(String name){
        return namedOutputs.getOrDefault(name, -1);
    }

    public int in(String name){
        ArrayUtils.AssertPresence(namedInputs.keySet(), name);
        return namedInputs.getOrDefault(name, -1);
    }

    public int out(String name){
        ArrayUtils.AssertPresence(namedOutputs.keySet(), name);
        return namedOutputs.getOrDefault(name, -1);
    }

    public static NamedComponent combinational(Component raw){
        return null;
    }

}
