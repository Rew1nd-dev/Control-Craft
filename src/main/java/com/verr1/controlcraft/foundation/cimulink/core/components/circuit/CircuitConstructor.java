package com.verr1.controlcraft.foundation.cimulink.core.components.circuit;


import com.verr1.controlcraft.foundation.cimulink.core.components.Component;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.sources.SignalGenerator;
import com.verr1.controlcraft.foundation.cimulink.core.records.ComponentPortName;
import com.verr1.controlcraft.foundation.cimulink.core.utils.GraphUtils;
import com.verr1.controlcraft.foundation.cimulink.core.records.ComponentPort;
import kotlin.Pair;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.stream.IntStream;

public class CircuitConstructor {
    private static final String INPUT_NAME = "input@#$%^";
    private static final String OUTPUT_NAME = "output@#$%^";

    Map<String, NamedComponent> components = new HashMap<>();

    Map<ComponentPortName, Set<ComponentPortName>> connections = new HashMap<>();
    Map<ComponentPortName, ComponentPortName> reverseConnections = new HashMap<>();

    Set<ComponentPortName> assigned = new HashSet<>();

    Map<String, Set<ComponentPortName>> inputs = new HashMap<>();
    Map<String, ComponentPortName> outputs = new HashMap<>();

    List<ComponentPortName> ordinal2outputPorts;

    List<String> inputNames;
    List<String> outputNames;

    public Circuit build(String name) {
        return (Circuit) (build().withName(name));
    }


    public Circuit build() {
        addComponent(INPUT_NAME, new Circuit.NamedInput(inputs.keySet().stream().toList()));
        addComponent(OUTPUT_NAME, new Circuit.NamedOutput(outputs.keySet().stream().toList()));
        Consumer<Pair<ComponentPortName, ComponentPortName>> connect = cp_cp -> connect(cp_cp.getFirst(), cp_cp.getSecond());
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

        /*
        Map<Integer, List<Integer>> componentId2outputWireIds = new HashMap<>();

        components.forEach((k, v) -> componentId2outputWireIds.put(
                componentName2componentId.get(k),
                v.outputs().stream().map(o -> new ComponentPortName(k, o)).map(outputPort2wireId::get).toList()
        ));
        * */

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

        Map<Integer, ComponentPort> wireId2outputPort = new HashMap<>();

        outputPort2wireId.forEach((k, v) -> {
            int cid = componentName2componentId.get(k.componentName());
            int pid = components.get(k.componentName()).namedOutputs().get(k.portName());
            wireId2outputPort.put(v, new ComponentPort(cid, pid));
        });


        List<ComponentPort> wireId2outputComponentPort = IntStream
                .range(0, wireId.get())
                .mapToObj(wireId2outputPort::get)
                .toList();

        List<List<ComponentPort>> wire2inputComponentIds = IntStream
                .range(0, wireId.get())
                .mapToObj(wireId2inputComponentIds::get)
                .toList();

        List<Component> allComponents = IntStream
                .range(0, componentId.get())
                .mapToObj(i -> (Component) components.get(componentId2componentName.get(i)))
                .toList();

        // filter out null elements, these are floating output wires
        List<Integer> ordinal2wireId = ordinal2outputPorts.stream().map(outputPort2wireId::get).filter(Objects::nonNull).toList();

        int inputId = componentName2componentId.get(INPUT_NAME);
        int outputId = componentName2componentId.get(OUTPUT_NAME);

        return new Circuit(
                inputNames, outputNames,
                inputId, outputId,
                allComponents,
                wireId2outputComponentPort,
                wire2inputComponentIds,
                ordinal2wireId // have not been implemented,
        );

    }

    public void computeOrdinal() {
        // construct a graph, nodes are outputPorts, arrows if
        // 1out --> {1in, 2in} --> {{1in --> 11out, 12out}, {2in --> 21out, 22out}}
        // then: 1out --> 11out, 12out, 21out, 22out
        Map<ComponentPortName, Set<ComponentPortName>> graph = new HashMap<>();
        connections.keySet().forEach(k -> graph.computeIfAbsent(k, $ -> new HashSet<>()));
        connections.forEach((cpo, cpis) ->
                cpis
                        .stream()
                        .flatMap(
                                cpi -> components
                                        .get(cpi.componentName())
                                        .propagateTo(cpi.portName())
                                        .stream()
                                        .map(outputPortName -> new ComponentPortName(cpi.componentName(), outputPortName))
                        )
                        .forEach(subCpo -> graph.computeIfAbsent(cpo, $ -> new HashSet<>()).add(subCpo)));

        // then, we can compute the ordinal of each outputPort
        ordinal2outputPorts = GraphUtils.TopologySort(graph);

    }

    public CircuitConstructor addComponent(String name, NamedComponent component) {
        if (components.containsKey(name)) {
            throw new IllegalArgumentException("component name already exist! : " + name);
        }
        // a little bit awkward

        components.put(name, component);


        if (component instanceof SignalGenerator<?> signalGenerator) {
            defineInput("@" + signalGenerator.name(), signalGenerator.__in(0));
        }

        return this;
    }

    public CircuitConstructor addComponent(NamedComponent... component) {
        for (var comp : component) {
            addComponent(comp.name(), comp);
        }
        return this;
    }

    public CircuitConstructor connect(
            String outputComponentName,
            String outputComponentPortName,
            String inputComponentName,
            String inputComponentPortName
    ) {
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

    public CircuitConstructor connect(ComponentPortName cpo, ComponentPortName cpi) {
        return connect(
                cpo.componentName(), cpo.portName(),
                cpi.componentName(), cpi.portName()
        );
    }

    private void checkOutputExistence(String componentName, String portName) {
        if (!components.containsKey(componentName)) {
            throw new IllegalArgumentException("Component not found: " + componentName);
        }
        NamedComponent component = components.get(componentName);
        if (!component.namedOutputs().containsKey(portName)) {
            throw new IllegalArgumentException("Output Port not found: " + portName + " in component " + componentName + " valid names: " + component.outputs());
        }
    }

    private void checkInputExistence(String componentName, String portName) {
        if (!components.containsKey(componentName)) {
            throw new IllegalArgumentException("Component not found: " + componentName);
        }
        NamedComponent component = components.get(componentName);
        if (!component.namedInputs().containsKey(portName)) {
            throw new IllegalArgumentException("Input Port not found: " + portName + " in component " + componentName + " valid names: " + component.inputs());
        }
    }

    private void checkAssigned(String componentName, String portName) {
        if (
                assigned.contains(new ComponentPortName(componentName, portName)) ||
                        inputs.getOrDefault(componentName, new HashSet<>()).contains(new ComponentPortName(componentName, portName))
        ) {
            throw new IllegalArgumentException("Input port " + portName + " of component " + componentName + " is already assigned to input.");
        }
    }

    private void assign(String componentName, String portName) {
        assigned.add(new ComponentPortName(componentName, portName));
    }

    public CircuitConstructor defineInput(String name, ComponentPortName cp) {
        return defineInput(name, cp.componentName(), cp.portName());
    }

    public CircuitConstructor defineOutput(String name, ComponentPortName cp) {
        return defineOutput(name, cp.componentName(), cp.portName());
    }

    public CircuitConstructor defineInput(
            String name,
            String componentName,
            String portName
    ) {
        checkInputExistence(componentName, portName);
        checkAssigned(componentName, portName);

        // assign(componentName, portName);

        inputs
                .computeIfAbsent(name, $ -> new HashSet<>())
                .add(new ComponentPortName(componentName, portName));
        return this;
    }

    public CircuitConstructor defineOutput(
            String name,
            String componentName,
            String portName
    ) {
        checkOutputExistence(componentName, portName);

        outputs.put(name, new ComponentPortName(componentName, portName));
        return this;
    }


}
