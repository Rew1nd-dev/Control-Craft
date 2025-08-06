package com.verr1.controlcraft.foundation.cimulink.standalone.eval;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class ValTransit {


    private final Evaluator evaluator;
    private final Map<String, Val> outs;
    private final Map<String, Val> ins;
    private final NamedComponent component;

    public ValTransit(List<Val> inputs, NamedComponent component, Evaluator evaluator) {
        this.outs = component.outputs().stream()
                .map(name -> new Val(this, name))
                .collect(Collectors.toMap(Val::portName, v -> v));

        this.ins = IntStream
                .range(0, inputs.size())
                .collect(
                        HashMap::new,
                        (map, i) -> map.put(component.in(i), inputs.get(i)),
                        HashMap::putAll
                );

        this.component = component;
        this.evaluator = evaluator;
    }

    public ValTransit(Map<String, Val> inputs, NamedComponent component, Evaluator evaluator) {
        this.outs = component.outputs().stream()
                .map(name -> new Val(this, name))
                .collect(Collectors.toMap(Val::portName, v -> v));

        inputs.keySet().forEach(component::in); // test if valid input
        this.ins = inputs;

        this.component = component;
        this.evaluator = evaluator;
    }


    public Evaluator evaluator() {
        return evaluator;
    }

    public Map<String, Val> outs() {
        return outs;
    }

    public Map<String, Val> ins() {
        return ins;
    }

    public NamedComponent component(){
        return component;
    }



    public Val out(){
        return outs().values().stream().findFirst().orElseThrow();
    }

}
