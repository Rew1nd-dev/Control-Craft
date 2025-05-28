package cimulink;

import java.util.Map;

public class NamedComponent {

    final Component unnamed;
    final Map<String, Integer> namedInputs; // name -> input array index
    final Map<String, Integer> namedOutputs;

    NamedComponent(
            Component unnamed,
            Map<String, Integer> namedInputs,
            Map<String, Integer> namedOutputs
    ) {
        this.unnamed = unnamed;
        this.namedInputs = namedInputs;
        this.namedOutputs = namedOutputs;
    }

}
