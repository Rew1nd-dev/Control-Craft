package com.verr1.controlcraft.foundation.cimulink.game.port.bus;

import com.google.common.collect.Sets;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.cimulink.core.components.Component;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import kotlin.Pair;
import org.jetbrains.annotations.NotNull;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

public class BusPort extends NamedComponent {

    private final IBusContext context;

    private final Set<String> allInNames;
    private final Set<String> allOutNames;
    private final Set<String> allNames;

    private final Map<String, Set<NamedComponent>> cache = new ConcurrentHashMap<>();

    public BusPort(
            List<String> definedInputs,
            List<String> definedOutputs,
            @NotNull IBusContext context
    ) {
        super(definedInputs, definedOutputs);
        this.context = context;
        allInNames = definedInputs.stream().map(n -> parse(n).getFirst()).collect(Collectors.toSet());
        allOutNames = definedOutputs.stream().map(n -> parse(n).getFirst()).collect(Collectors.toSet());
        allNames = new HashSet<>(allInNames);
        allNames.addAll(allOutNames);
    }

    public static Pair<String, String> parse(String name){
        String[] s = name.split(":", 2);
        if(s.length < 2)return new Pair<>(name, "");
        return new Pair<>(s[0], s[1]);
    }

    public static String compress(String name, String port){
        return name + ":" + port;
    }

    @Override
    public List<Integer> propagateTo(int inputIndex) {
        return List.of();
    }

    @Override
    public void onInputChange(Integer... inputIndexes) {}

    public void updateCache(){
        cache.clear();

        allNames.forEach(name -> {
            Set<NamedComponent> components = context.access(name);
            cache.put(name, Sets.newConcurrentHashSet(components));
        });
    }

    @Override
    public void onPositiveEdge() {

        changedInput().forEach(i -> {
            double val = retrieveInput(i);
            Pair<String, String> parsed = parse(in(i));
            String name = parsed.getFirst();
            String port = parsed.getSecond();
            cache.getOrDefault(name, Collections.emptySet())
                    .stream()
                    .filter(c -> c.hasInput(port))
                    .forEach(c -> {
                        c.input(port, val);
                        c.onInputChange(c.in(port));
                    });
        });

//        try{
//            cache.values().stream().flatMap(Collection::stream).forEach(c -> {
//                try{
//                    c.onPositiveEdge();
//                }catch (RuntimeException e){
//                    ControlCraft.LOGGER.error("Error During Temporal Propagation At : {}, {}", c.getClass(), e.getMessage());
//                    throw e;
//                }
//            });
//        } catch (RuntimeException e) {
//            ControlCraft.LOGGER.error("Error During Temporal Propagation At BusPort: {}, {}", e.getCause(), e.getMessage());
//            throw new RuntimeException(e);
//        }

        allOutNames.forEach(s -> cache
            .getOrDefault(s, Collections.emptySet())
            .forEach(c -> {
                c.outputs().forEach(outPort -> {
                   double val = c.peekOutput(outPort);
                   // String outPort = c.out(i);
                   String compressed = compress(s, outPort);
                   if(hasOutput(compressed)){
                       updateOutput(out(compressed), val);
                   }
                });
            }));

    }


}
