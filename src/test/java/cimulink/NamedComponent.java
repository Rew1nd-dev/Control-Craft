package cimulink;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class NamedComponent {

    final Component unnamed;
    final Map<String, Integer> namedInputs; // name -> input array index
    final Map<String, Integer> namedOutputs;



    final List<String> inputs;
    final List<String> outputs;

    NamedComponent(
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

    public List<String> inputs() {
        return inputs;
    }

    public List<String> outputs() {
        return outputs;
    }

    public int outputId(String name){
        return namedOutputs.getOrDefault(name, -1);
    }

    public int inputId(String name){
        return namedInputs.getOrDefault(name, -1);
    }

}
