package cimulink;

import cimulink.utils.GraphUtils;
import kotlin.Pair;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;


public class Circuit {
    public static final int Z_ID = -1;
    public static final int INPUT_COMP_ID = -2;

    final List<Wire> wires              ;
    final List<Component> components    ;

    final List<Integer>       wire2InputComponentID          ; // Wires Are Suppose Connected To Some Component Output Port

                                                             ;
    final List<List<Integer>> comp2inputWireIDs              ;
    final List<List<Integer>> comp2outputWireIDs             ;
                                                             ;
    final Map<String, Integer> inputWires                    ;
    final Map<String, Integer> outputWires                   ;



    final List<String> inputs;
    final List<String> outputs;

    /** Contract:
     * <p>
     *      1.  Assuming All Temporal Components Sample At Positive Edges
     * <p>
     *          --> Temporal Components not here
     * <p>
     *
     * <p>
     *     Because: Circuit forward() stops at temporal component input wires
     *
    * */
    final List<List<Integer>> ordinal2comps                  ;

    final List<Integer> temporalComponents;

    private Circuit(
            List<Wire> wires,
            List<Component> components,
            List<Integer> wire2InputComponentID,
            List<List<Integer>> comp2inputWireIDs,
            List<List<Integer>> comp2outputWireIDs,
            Map<String, Integer> inputWires,
            Map<String, Integer> outputWires,
            List<Integer> temporalComponents,
            List<List<Integer>> ordinal2comps
    ) {
        this.wires = wires;
        this.components = components;
        this.wire2InputComponentID = wire2InputComponentID;
        this.comp2inputWireIDs = comp2inputWireIDs;
        this.comp2outputWireIDs = comp2outputWireIDs;
        this.inputWires = inputWires;
        this.outputWires = outputWires;
        // this.ordinal2wires = ordinal2wires;
        this.ordinal2comps = ordinal2comps;

        this.temporalComponents = temporalComponents;
        this.inputs = inputWires.keySet().stream().sorted().toList();
        this.outputs = outputWires.keySet().stream().sorted().toList();


    }

    public boolean anyTemporalOutput(){
        return temporalComponents
                    .stream()
                    .flatMap(tci -> comp2outputWireIDs.get(tci).stream())
                    .anyMatch(outputWires::containsValue);
    }


    public Circuit input(String name, double value){
        if(!inputWires.containsKey(name)){
            throw new IllegalArgumentException("Circuit Does Not Contain Input Wire: " + name);
        }
        wires.get(inputWires.get(name)).sample = value;
        return this;
    }

    public double output(String name){
        if(!outputWires.containsKey(name)){
            throw new IllegalArgumentException("Circuit Does Not Contain Output Wire: " + name);
        }
        return wires.get(outputWires.get(name)).sample;
    }

    public List<String> inputs() {
        return inputs;
    }

    public List<String> outputs() {
        return outputs;
    }


    public Circuit forward(){
        // Input Coming (input() should be called before forward())
        // Before Positive Edge Coming Input Supply New Inputs, Combinational Should Forward Themselves
        forwardCombinational();
        // Actual Positive Edge
        forwardTemporal();
        // After Positive Edge, Temporal Supply New Inputs, Combinational Should Forward Themselves
        forwardCombinational();
        // All Logic Are Done
        // Call output() to get output values
        return this;
    }

    private void forwardTemporal(){
        // This is not done by mimicking forwardCombinational()
        // Because Temporal Components May Connect Serially, Which Causes Inconsistent Behavior If Using Methods of Combinational
        // (i.e. forEach sequence matters)
        // So Always Consider Their Transition Starts Only After All Temporal Components Finish Input Snapshot
        // Which Makes Sense, Considering Gate Delay In Reality :)


        // Temporal Components Snapshot Their Input On Positive Edges
        temporalComponents.forEach(this::snapTemporal);
        // After All Temporal Components Finish Their Snapshot, They Transit To Next State And Supply Output
        temporalComponents.forEach(this::transitTemporal);
    }

    private void forwardCombinational(){
        // ordinal is computed before Circuit is built
        // It's guaranteed that combinational components' inputs are up-to-date
        // if they are updated by ordinals
        for (List<Integer> comps : ordinal2comps) {
            comps.forEach(this::forwardCombinational);
        }
    }

    private void forwardCombinational(int c){
        List<Double> samples = sampleInput(c);
        Component comp = components.get(c);
        comp.consume(samples);
        comp.transit();
        List<Double> outputs = comp.supply();
        supplyOutputs(c, outputs);
    }

    private List<Double> sampleInput(int c){
        return comp2inputWireIDs
                // This Will Help Comp To Find Their Inputs, Including Global Inputs
                .get(c)
                .stream()
                .map(wid -> wid == Z_ID ? 0 : wires.get(wid).sample)
                .toList();
    }

    private void supplyOutputs(int c, List<Double> outputs){
        List<Integer> outputWireIDs = comp2outputWireIDs.get(c);
        for (int i = 0; i < outputs.size(); i++) {
            int wireID = outputWireIDs.get(i);
            if(wireID == Z_ID)continue;
            wires.get(wireID).sample = outputs.get(i);
        }
    }

    private void snapTemporal(int c){
        List<Double> samples = sampleInput(c);
        Component comp = components.get(c);
        comp.consume(samples);
    }

    private void transitTemporal(int c){
        Component comp = components.get(c);
        comp.transit();
        List<Double> outputs = comp.supply();
        supplyOutputs(c, outputs);
    }




    public void validate(){

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


    public static class builder{
        Map<String, NamedComponent> components = new HashMap<>();

        Map<ComponentPort, Set<ComponentPort>> connections = new HashMap<>();
        Map<ComponentPort, ComponentPort> reverseConnections = new HashMap<>();

        Set<ComponentPort> assigned = new HashSet<>();

        Map<String, Set<ComponentPort>> inputs = new HashMap<>();
        Map<String, ComponentPort> outputs = new HashMap<>();

        Map<String, Integer> ordinal = new HashMap<>(); // component2ordinal
        int maxOrdinal = 0;

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

            reverseConnections.put(
                    new ComponentPort(inputComponentName, inputComponentPortName),
                    new ComponentPort(outputComponentName, outputComponentPortName)
            );
            return this;
        }

        private void checkOutputExistence(String componentName, String portName){
            if (!components.containsKey(componentName)) {
                throw new IllegalArgumentException("Component not found: " + componentName);
            }
            NamedComponent component = components.get(componentName);
            if (!component.namedOutputs.containsKey(portName)) {
                throw new IllegalArgumentException("Output Port not found: " + portName + " in component " + componentName + " valid names: " + component.outputs);
            }
        }

        private void checkInputExistence(String componentName, String portName){
            if (!components.containsKey(componentName)) {
                throw new IllegalArgumentException("Component not found: " + componentName);
            }
            NamedComponent component = components.get(componentName);
            if (!component.namedInputs.containsKey(portName)) {
                throw new IllegalArgumentException("Input Port not found: " + portName + " in component " + componentName + " valid names: " + component.inputs());
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

            inputs
                    .computeIfAbsent(name, $ -> new HashSet<>())
                    .add(new ComponentPort(componentName, portName));
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
            Map<String, List<String>> graph = new HashMap<>();
            Set<String> zeroOrderVertices = components
                    .keySet()
                    .stream()
                    .filter(this::isZeroOrder)
                    .collect(Collectors.toSet());

            components
                    .keySet().forEach(
                            k -> graph.computeIfAbsent(k, $ -> new ArrayList<>()).addAll(getSubComponents(k))
                    );


            ordinal = GraphUtils.calculateOrders(
                    graph,
                    name -> !components.get(name).unnamed.immediate,
                    zeroOrderVertices::contains
            );

            maxOrdinal = ordinal.values().stream().max(Comparator.naturalOrder()).orElse(0) + 1;
        }

        private boolean isZeroOrder(String name){
            NamedComponent namedComponent = components.get(name);
            if(!namedComponent.unnamed.immediate){
                return true;
            }else{
                return !namedComponent
                        .inputs()
                        .stream()
                        .map(i -> reverseConnections.containsKey(new ComponentPort(name, i)))
                        .findAny()
                        .orElse(false);
            }
        }

        private Set<String> getSubComponents(String componentName){
            return components
                    .get(componentName)
                    .outputs()
                    .stream()
                    .flatMap(o -> connections
                            .getOrDefault(new ComponentPort(componentName, o), Set.of())
                            .stream()
                            .map(cp -> cp.componentName)
                    )
                    .collect(Collectors.toSet());
        }

        public Circuit build(){
            computeOrdinal();
            Map<String, Integer> componentName2componentId = new HashMap<>();
            Map<ComponentPort, Integer> outputPort2wireId = new HashMap<>();
            Map<String, Integer> outputName2wireId = new HashMap<>();
            Map<String, Integer> inputName2wireId = new HashMap<>(); // for global input

            AtomicInteger componentId = new AtomicInteger(0);
            components.keySet().forEach(k -> componentName2componentId.put(k, componentId.getAndIncrement()));
            AtomicInteger wireId = new AtomicInteger(0);
            inputs.keySet().forEach(p -> inputName2wireId.put(p, wireId.getAndIncrement()));
            outputs.keySet().forEach(p -> outputName2wireId.put(p, wireId.getAndIncrement()));
            connections
                    .keySet()
                    .forEach(k -> outputPort2wireId.put(k, wireId.getAndIncrement()));

            outputs.forEach((k, v) -> outputPort2wireId.put(v, Objects.requireNonNull(outputName2wireId.get(k))));

            Map<ComponentPort, Integer> inputPort2wireId = new HashMap<>();
            inputs
                    .entrySet()
                    .stream()
                    .flatMap(e -> e.getValue().stream().map(v -> new Pair<>(e.getKey(), v)))
                    .forEach(kv -> inputPort2wireId.put(kv.getSecond(), Objects.requireNonNull(inputName2wireId.get(kv.getFirst()))));

            connections
                    .entrySet()
                    .stream()
                    .flatMap(e -> e.getValue().stream().map(v -> new Pair<>(e.getKey(), v)))
                    .forEach(kv -> inputPort2wireId.put(kv.getSecond(), Objects.requireNonNull(outputPort2wireId.get(kv.getFirst()))));


            Map<Integer, List<Integer>> compId2inputWireIds = new HashMap<>();
            Map<Integer, List<Integer>> compId2outputWireIds = new HashMap<>();
            for(var e: components.entrySet()){
                NamedComponent comp = e.getValue();
                String name = e.getKey();

                List<Integer> inputIndex2wireId = comp
                        .inputs()
                        .stream()
                        .map(i -> new ComponentPort(name, i))
                        .map(cp -> inputPort2wireId.getOrDefault(cp, Z_ID))
                        .toList();

                List<Integer> outputIndex2wireId = comp
                        .outputs()
                        .stream()
                        .map(i -> new ComponentPort(name, i))
                        .map(cp -> outputPort2wireId.getOrDefault(cp, Z_ID))
                        .toList();

                compId2outputWireIds.put(componentName2componentId.get(name), outputIndex2wireId);
                compId2inputWireIds.put(componentName2componentId.get(name), inputIndex2wireId);
            }

            Map<Integer, ComponentPort> wireId2outputPort = new HashMap<>();
            outputPort2wireId.forEach((k, v) -> wireId2outputPort.put(v, k));

            Map<Integer, String> componentId2componentName = new HashMap<>();
            componentName2componentId.forEach((k, v) -> componentId2componentName.put(v, k));


            Map<Integer, Set<Integer>> ordinal2compId = new HashMap<>();


            Function<String, Boolean> isImmediate = s -> components.get(s).unnamed.immediate;
            // Temporal Component Are Not Added To Ordinal Array
            // Setting Temporal Component To 0-ordered Is A Trick To Help Compute Ordinals
            ordinal
                    .entrySet()
                    .stream()
                    .filter(e -> isImmediate.apply(e.getKey()))
                    .forEach(
                            e -> ordinal2compId
                                        .computeIfAbsent(e.getValue(), $ -> new HashSet<>())
                                        .add(componentName2componentId.get(e.getKey()))
                    );

            List<Integer> wire2InputComponentID = IntStream
                    .range(0, wireId.get())
                    .map(i -> Optional
                            // may be connected to global input port
                                .ofNullable(wireId2outputPort.get(i))
                                .map(p -> p.componentName)
                                .map(componentName2componentId::get)
                                .orElse(INPUT_COMP_ID)
                    )
                    .boxed()
                    .toList();

            List<List<Integer>> comp2inputWireIDs = IntStream
                    .range(0, components.size())
                    .mapToObj(compId2inputWireIds::get)
                    .toList();

            List<List<Integer>> comp2outputWireIDs = IntStream
                    .range(0, components.size())
                    .mapToObj(compId2outputWireIds::get)
                    .toList();

            List<Wire> wires = IntStream
                    .range(0, wireId.get())
                    .mapToObj($ -> new Wire())
                    .toList();

            List<Component> comps = IntStream
                    .range(0, componentId.get())
                    .mapToObj(i -> components.get(componentId2componentName.get(i)).unnamed)
                    .toList();

            List<List<Integer>> ordinal2comps = IntStream
                    .range(0, maxOrdinal)
                    .mapToObj(o -> ordinal2compId.get(o).stream().toList())
                    .toList();

            List<Integer> temporalComponents = IntStream
                    .range(0, comps.size())
                    .filter(i -> !comps.get(i).immediate)
                    .boxed()
                    .toList();
            // ordinal2comps.get(0).stream().filter(isImmediate::apply).toList();
            // need test
            return new Circuit(
                    wires,
                    comps,
                    wire2InputComponentID,
                    comp2inputWireIDs,
                    comp2outputWireIDs,
                    inputName2wireId,
                    outputName2wireId,
                    temporalComponents,
                    ordinal2comps
            );
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
