package com.verr1.controlcraft.foundation.cimulink.game.port.bus;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public interface IBusContext {

    @NotNull
    Set<NamedComponent> access(String name);

    @NotNull
    Set<String> allNames();

    default Map<String, Set<String>> allOutputPorts(){
        Map<String, Set<String>> result = new HashMap<>();
        allNames().forEach(n -> result.put(n, access(n).stream().flatMap(c -> c.outputs().stream()).collect(Collectors.toSet())));
        return result;
    }

    default Map<String, Set<String>> allInputPorts(){
        Map<String, Set<String>> result = new HashMap<>();
        allNames().forEach(n -> result.put(n, access(n).stream().flatMap(c -> c.inputs().stream()).collect(Collectors.toSet())));
        return result;
    }

}
