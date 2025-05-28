package cimulink;

import java.util.*;
import java.util.stream.Collectors;

public class Circuit {
    public static final int Z_ID = -1;

    List<Wire> wires = new ArrayList<>();
    List<Component> components = new ArrayList<>();

    List<Integer>       wire2InputComponentID = new ArrayList<>();
    // List<List<Integer>> wire2OutputComponentIDs = new ArrayList<>();

    List<List<Integer>> comp2inputWireIDs = new ArrayList<>();
    List<List<Integer>> comp2outputWireIDs = new ArrayList<>();

    Map<String, Integer> inputWires = new HashMap<>();
    Map<String, Integer> outputWires = new HashMap<>();

    // List<Integer> wire2ordinal = new ArrayList<>();
    List<List<Integer>> ordinal2wires = new ArrayList<>();


    public void input(String name, double value){
        if(!inputWires.containsKey(name)){
            throw new IllegalArgumentException("Circuit Does Not Contain Input Wire: " + name);
        }
        wires.get(inputWires.get(name)).sample = value;
    }

    public double output(String name){
        if(!outputWires.containsKey(name)){
            throw new IllegalArgumentException("Circuit Does Not Contain Output Wire: " + name);
        }
        return wires.get(outputWires.get(name)).sample;
    }

    public void forward(){
        for (List<Integer> ordinal2wire : ordinal2wires) {

            Set<Integer> comps = ordinal2wire
                    .stream()
                    .map(wid -> wire2InputComponentID.get(wid))
                    .collect(Collectors.toSet());

            comps.forEach(
                    c -> {
                        List<Double> samples = comp2inputWireIDs
                                .get(c)
                                .stream()
                                .map(wid -> wid == Z_ID ? 0 : wires.get(wid).sample)
                                .toList();
                        Component comp = components.get(c);
                        comp.consume(samples);
                        if (!comp.immediate) return;
                        List<Double> outputs = comp.supply();
                        List<Integer> outputWireIDs = comp2outputWireIDs.get(c);
                        for (int i = 0; i < outputs.size(); i++) {
                            int wireID = outputWireIDs.get(i);
                            wires.get(wireID).sample = outputs.get(i);
                        }

                    }
            );
        }
    }




    public void validate(){
        /*
        * wire2InputComponentID
                .stream()
                .filter(cid -> cid >= components.size() || cid < 0)
                .findAny()
                .ifPresent(
                        t -> {
                            throw new IllegalArgumentException("Invalid component ID in wire2InputComponentID: " + t);
                        }
                );

        wire2OutputComponentIDs
                .stream()
                .flatMap(Collection::stream)
                .filter(cid -> cid >= components.size() || cid < 0)
                .findAny()
                .ifPresent(
                        t -> {
                            throw new IllegalArgumentException("Invalid component ID in wire2OutputComponentID: " + t);
                        }
                );
        * */

        comp2inputWireIDs
                .stream()
                .flatMap(Collection::stream)
                .filter(cid -> cid >= wires.size() || cid < 0)
                .findAny()
                .ifPresent(
                        t -> {
                            throw new IllegalArgumentException("Invalid wire ID in wire2InputWireID: " + t);
                        }
                );

        comp2outputWireIDs
                .stream()
                .flatMap(Collection::stream)
                .filter(cid -> cid >= wires.size() || cid < 0)
                .findAny()
                .ifPresent(
                        t -> {
                            throw new IllegalArgumentException("Invalid wire ID in comp2OutputWireID: " + t);
                        }
                );
    }


    static class builder{
        Map<String, NamedComponent> components = new HashMap<>();

        Map<ComponentPort, Set<ComponentPort>> connections = new HashMap<>();

        Set<ComponentPort> assigned = new HashSet<>();

        Map<String, ComponentPort> inputs = new HashMap<>();
        Map<String, ComponentPort> outputs = new HashMap<>();

        Map<ComponentPort, Integer> ordinal = new HashMap<>();

        public builder addComponent(String name, NamedComponent component){
            components.put(name, component);
            return this;
        }

        public builder connect(
                String outputComponentName,
                String outputComponentPortName,
                String inputComponentName,
                String inputComponentPortName
        ){
            checkOutputExistence(outputComponentName, outputComponentPortName);
            checkInputExistence(inputComponentName, inputComponentPortName);
            checkAssigned(inputComponentName, inputComponentPortName);


            assign(inputComponentName, inputComponentPortName);

            connections
                    .computeIfAbsent(
                        new ComponentPort(outputComponentName, outputComponentPortName),
                        $ -> new HashSet<>()
                    ).add(new ComponentPort(inputComponentName, inputComponentPortName));

            return this;
        }

        private void checkOutputExistence(String componentName, String portName){
            if (!components.containsKey(componentName)) {
                throw new IllegalArgumentException("Component not found: " + componentName);
            }
            NamedComponent component = components.get(componentName);
            if (!component.namedOutputs.containsKey(portName)) {
                throw new IllegalArgumentException("Output Port not found: " + portName + " in component " + componentName);
            }
        }

        private void checkInputExistence(String componentName, String portName){
            if (!components.containsKey(componentName)) {
                throw new IllegalArgumentException("Component not found: " + componentName);
            }
            NamedComponent component = components.get(componentName);
            if (!component.namedInputs.containsKey(portName)) {
                throw new IllegalArgumentException("Input Port not found: " + portName + " in component " + componentName);
            }
        }

        private void checkAssigned(String componentName, String portName){
            if (assigned.contains(new ComponentPort(componentName, portName))){
                throw new IllegalArgumentException("Input port " + portName + " of component " + componentName + " is already assigned to input.");
            }
        }

        private void assign(String componentName, String portName){
            assigned.add(new ComponentPort(componentName, portName));
        }

        public builder defineInput(
                String name,
                String componentName,
                String portName
        ){
            checkInputExistence(componentName, portName);
            checkAssigned(componentName, portName);

            assign(componentName, portName);

            inputs.put(name, new ComponentPort(componentName, portName));
            return this;
        }

        public builder defineOutput(
                String name,
                String componentName,
                String portName
        ){
            checkOutputExistence(componentName, portName);

            outputs.put(name, new ComponentPort(componentName, portName));
            return this;
        }

        private void computeOrdinal(){

        }

        public Circuit build(){
            List<NamedComponent> circuitComponents = new ArrayList<>();
            Map<String, Integer> component2Id = new HashMap<>();
            for(var e : components.entrySet()){
                NamedComponent component = e.getValue();
                circuitComponents.add(component);
                component2Id.put(e.getKey(), circuitComponents.size() - 1);
            }

            List<Wire> wires = new ArrayList<>();
            Map<ComponentPort, Integer> port2WireId = new HashMap<>();

            for (var e: connections.entrySet()){
                ComponentPort outputPort = e.getKey();
                // Create a wire for this connection
                wires.add(new Wire());
                // Map the ports to the wire ID
                port2WireId.put(outputPort, wires.size() - 1);
            }


            List<Integer>       wire2InputComponentID = new ArrayList<>();


            List<List<Integer>> comp2inputWireIDs = new ArrayList<>();
            List<List<Integer>> comp2outputWireIDs = new ArrayList<>();

            Map<String, Integer> inputWires = new HashMap<>();
            Map<String, Integer> outputWires = new HashMap<>();

            for (int i = 0; i < wires.size(); i++) {
                wire2InputComponentID.add(Z_ID);
            }

            for (int i = 0; i < circuitComponents.size(); i++) {
                comp2inputWireIDs.add(new ArrayList<>());
                comp2outputWireIDs.add(new ArrayList<>());
            }

            for (var e: connections.entrySet()){
                ComponentPort outputPort = e.getKey();


                int wireId = port2WireId.get(outputPort);
                int outputId = component2Id.get(outputPort.componentName);
                wire2InputComponentID.set(wireId, outputId);
            }

            for(var e: components.entrySet()){
                int componentId = component2Id.get(e.getKey());
                NamedComponent component = circuitComponents.get(componentId);
                component.namedOutputs.entrySet().forEach(
                        oe -> {
                            String portName = oe.getKey();
                            int outputPortIndex = oe.getValue();
                            /*
                            ComponentPort port = new ComponentPort(e.getKey(), outputPort);
                            Set<ComponentPort> connectedPorts = connections.get(port);
                            if (connectedPorts != null) {

                            }
                            * */

                        }
                );


            }

            return null;
        }

        record ComponentPort(String componentName, String portName){
            @Override
            public boolean equals(Object obj) {
                if(obj instanceof ComponentPort other){
                    return componentName.equals(other.componentName) &&
                           portName.equals(other.portName);
                }
                return false;
            }

            @Override
            public int hashCode() {
                return componentName.hashCode() ^ portName.hashCode();
            }
        }
    }
}
