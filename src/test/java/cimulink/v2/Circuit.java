package cimulink.v2;

import cimulink.v2.components.Component;
import cimulink.v2.components.NamedComponent;
import kotlin.Pair;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Circuit extends NamedComponent {

    public static final Supplier<Component> PLACE_HOLDER = PlaceHolder::new;

    List<Component> components;

    List<List<Integer>> component2outputWireIds;
    List<List<ComponentPort>> wireId2inputComponentIds;

    List<Integer> ordinal2componentId;
    List<Boolean> isImmediate;

    // Contract: The First And Second Component Should Be Circuit Input And Output Component,
    // But Initializing Helper Needs "this", So Put A PlaceHolder At Builder
    private Circuit(
            List<String> inputNames, List<String> outputNames,
            int inputId, int outputId,
            List<Component> components,
            List<List<Integer>> component2outputWireIds,
            List<List<ComponentPort>> wireId2inputComponentIds,
            List<Integer> ordinal2componentId
    ) {
        super(inputNames, outputNames);
        components.set(inputId, new CircuitInputComponent());
        components.set(outputId, new CircuitOutputComponent());
        this.components = components;
        this.component2outputWireIds = component2outputWireIds;
        this.wireId2inputComponentIds = wireId2inputComponentIds;
        this.ordinal2componentId = ordinal2componentId;

        Boolean[] isImmediateArray = new Boolean[m()];
        for(int i = 0; i < n(); i++)input(i, 0.0);
        onInputChange();
        changedInput().forEach(i -> isImmediateArray[i] = true);
        isImmediate = List.of(isImmediateArray);
    }



    @Override
    public void onInputChange() {
        propagate();
    }

    private void propagate(){
        for(int cid: ordinal2componentId){
            if(!needPropagate(cid))continue;
            components.get(cid).onInputChange();
            for(var pid: components.get(cid).changedOutput()){
                retrieveAndPropagate(cid, pid);
            }
        }
    }

    private boolean needPropagate(int componentId){
        return components.get(componentId).anyInputChanged();
    }

    /*private void propagateInput(){
        List<Integer> changedPort = changedInput();
        List<Double> inputs = retrieveInput();
        for(int portId: changedPort){
            double inputValue = inputs.get(portId);
            int wireId = inputId2wireId.get(portId);
            List<ComponentPort> affectedPorts = wireId2inputComponentIds.get(wireId);
            for(var affected: affectedPorts){
                components.get(affected.componentId).input(affected.portId(), inputValue);
            }
        }
    }
        *
        * */


    private void retrieveAndPropagate(int componentId, int portId){
        double value = components.get(componentId).retrieveOutput(portId);
        int wireId = component2outputWireIds.get(componentId).get(portId);
        for(ComponentPort affectedComponent: wireId2inputComponentIds.get(wireId)){
            components.get(affectedComponent.componentId()).input(affectedComponent.portId(), value);
        }
    }

    @Override
    public void onPositiveEdge() {
        components.forEach(Component::onPositiveEdge);
        propagate();
    }

    @Override
    protected boolean immediateInternal(int index) {
        return isImmediate.get(index);
    }

    public void cycle(){
        onInputChange();
        onPositiveEdge();
        onInputChange();
    }

    private class CircuitInputComponent extends PlaceHolder{
        @Override
        public boolean anyInputChanged() {
            // starts propagating
            return Circuit.this.anyInputChanged();
        }

        @Override
        public List<Integer> changedOutput() {
            return Circuit.this.changedInput();
        }

        @Override
        public List<Double> retrieveOutput() {
            return Circuit.this.retrieveInput();
        }

    }

    private class CircuitOutputComponent extends PlaceHolder{
        @Override
        public Component input(int index, double value) {
            Circuit.this.updateOutput(index, value);
            return this;
        }

        @Override
        public boolean anyOutputChanged() {
            // Stops Propagation
            return false;
        }
    }

    private static class PlaceHolder extends Component{
        public PlaceHolder() {
            super(0, 0);
        }
        @Override
        public void onInputChange() {}
        @Override
        public void onPositiveEdge() {}
        @Override
        protected boolean immediateInternal(int index) {return false;}
    }

    private static class NamedInput extends NamedComponent{
        public NamedInput(List<String> outputs) {
            super(List.of(), outputs);
        }
        @Override
        public void onInputChange() {}
        @Override
        public void onPositiveEdge() {}
        @Override
        protected boolean immediateInternal(int index) {
            return false;
        }
    }

    private static class NamedOutput extends NamedComponent{
        public NamedOutput(List<String> inputs) {
            super(inputs, List.of());
        }
        @Override
        public void onInputChange() {}
        @Override
        public void onPositiveEdge() {}
        @Override
        protected boolean immediateInternal(int index) {return false;}
    }


    public static class builder{
        private static final String INPUT_NAME = "input@#$%^";
        private static final String OUTPUT_NAME = "output@#$%^";

        Map<String, NamedComponent> components = new HashMap<>();

        Map<ComponentPortName, Set<ComponentPortName>> connections = new HashMap<>();
        Map<ComponentPortName, ComponentPortName> reverseConnections = new HashMap<>();

        Set<ComponentPortName> assigned = new HashSet<>();

        Map<String, Set<ComponentPortName>> inputs = new HashMap<>();
        Map<String, ComponentPortName> outputs = new HashMap<>();

        Map<String, Integer> ordinal = new HashMap<>();

        List<String> inputNames;
        List<String> outputNames;
        /*
        List<Boolean> isImmediate;

        private void computeImmediate(){
            Set<ComponentPortName> outputPorts = new HashSet<>(reverseConnections.values());
            Set<ComponentPortName> inputPorts = reverseConnections.keySet();
            Map<ComponentPortName, Boolean> outputIsImmediate = new HashMap<>();

        }
        * */

        public Circuit build(){
            addComponent(INPUT_NAME, new NamedInput(inputs.keySet().stream().toList()));
            addComponent(INPUT_NAME, new NamedOutput(outputs.keySet().stream().toList()));
            Consumer<Pair<ComponentPortName, ComponentPortName>> connect = cp_cp -> connect(
                    cp_cp.getFirst().componentName, cp_cp.getFirst().portName,
                    cp_cp.getSecond().componentName, cp_cp.getSecond().portName
            );
            inputNames = inputs.keySet().stream().toList();
            outputNames = outputs.keySet().stream().toList();
            inputNames
                .stream()
                .flatMap(e -> inputs
                        .get(e)
                        .stream()
                        .map(cp -> new Pair<>(new ComponentPortName(INPUT_NAME, e), cp))
                ).forEach(connect);

            outputNames
                .stream()
                .map(e -> new Pair<>(outputs.get(e), new ComponentPortName(OUTPUT_NAME, e)))
                .forEach(connect);

            // computeImmediate();
            computeOrdinal();

            Map<String, Integer> componentName2componentId = new HashMap<>();
            Map<ComponentPortName, Integer> outputPort2wireId = new HashMap<>();

            AtomicInteger componentId = new AtomicInteger(0);
            components.keySet().forEach(k -> componentName2componentId.put(k, componentId.getAndIncrement()));
            AtomicInteger wireId = new AtomicInteger(0);
            connections
                    .keySet()
                    .forEach(k -> outputPort2wireId.put(k, wireId.getAndIncrement()));

            Map<Integer, List<Integer>> componentId2outputWireIds = new HashMap<>();

            components.forEach((k, v) -> componentId2outputWireIds.put(
                    componentName2componentId.get(k),
                    v.outputs().stream().map(o -> new ComponentPortName(k, o)).map(outputPort2wireId::get).toList()
            ));

            Map<Integer, List<ComponentPort>> wireId2inputComponentIds = new HashMap<>();

            connections.forEach((cpo, cpi) -> wireId2inputComponentIds
                    .put(
                            outputPort2wireId.get(cpo),
                            cpi
                                .stream()
                                .map(cpin -> new ComponentPort(
                                    componentName2componentId.get(cpin.componentName()),
                                    components.get(cpin.componentName()).namedInputs().get(cpin.portName())
                                )).toList()
                    ));

            Map<Integer, String> componentId2componentName = new HashMap<>();
            componentName2componentId.forEach((k, v) -> componentId2componentName.put(v, k));

            List<List<Integer>> component2outputWireIds = IntStream
                            .range(0, componentId.get())
                            .mapToObj(componentId2outputWireIds::get)
                            .toList();

            List<List<ComponentPort>> wire2inputComponentIds = IntStream
                    .range(0, wireId.get())
                    .mapToObj(wireId2inputComponentIds::get)
                    .toList();

            List<Component> components_ = IntStream
                    .range(0, componentId.get())
                    .mapToObj(i -> (Component)components.get(componentId2componentName.get(i)))
                    .toList();

            int inputId = componentName2componentId.get(INPUT_NAME);
            int outputId = componentName2componentId.get(OUTPUT_NAME);

            return new Circuit(
                    inputNames, outputNames,
                    inputId, outputId,
                    components_,
                    component2outputWireIds,
                    wire2inputComponentIds,
                    List.of() // have not been implemented,
            );

        }

        public void computeOrdinal(){}

        public builder addComponent(String name, NamedComponent component){
            if(components.containsKey(name)){
                throw new IllegalArgumentException("component name already exist! : " + name);
            }
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
                            new ComponentPortName(outputComponentName, outputComponentPortName),
                            $ -> new HashSet<>()
                    ).add(new ComponentPortName(inputComponentName, inputComponentPortName));

            reverseConnections.put(
                    new ComponentPortName(inputComponentName, inputComponentPortName),
                    new ComponentPortName(outputComponentName, outputComponentPortName)
            );
            return this;
        }

        private void checkOutputExistence(String componentName, String portName){
            if (!components.containsKey(componentName)) {
                throw new IllegalArgumentException("Component not found: " + componentName);
            }
            NamedComponent component = components.get(componentName);
            if (!component.namedOutputs().containsKey(portName)) {
                throw new IllegalArgumentException("Output Port not found: " + portName + " in component " + componentName + " valid names: " + component.outputs());
            }
        }

        private void checkInputExistence(String componentName, String portName){
            if (!components.containsKey(componentName)) {
                throw new IllegalArgumentException("Component not found: " + componentName);
            }
            NamedComponent component = components.get(componentName);
            if (!component.namedInputs().containsKey(portName)) {
                throw new IllegalArgumentException("Input Port not found: " + portName + " in component " + componentName + " valid names: " + component.inputs());
            }
        }

        private void checkAssigned(String componentName, String portName){
            if (assigned.contains(new ComponentPortName(componentName, portName))){
                throw new IllegalArgumentException("Input port " + portName + " of component " + componentName + " is already assigned to input.");
            }
        }

        private void assign(String componentName, String portName){
            assigned.add(new ComponentPortName(componentName, portName));
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
                    .add(new ComponentPortName(componentName, portName));
            return this;
        }

        public builder defineOutput(
                String name,
                String componentName,
                String portName
        ){
            checkOutputExistence(componentName, portName);

            outputs.put(name, new ComponentPortName(componentName, portName));
            return this;
        }


    }

    private record ComponentPort(int componentId, int portId){}

    private record ComponentPortName(String componentName, String portName){}

}
