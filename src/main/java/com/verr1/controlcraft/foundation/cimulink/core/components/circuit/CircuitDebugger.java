package com.verr1.controlcraft.foundation.cimulink.core.components.circuit;


import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.records.ComponentPortName;
import com.verr1.controlcraft.foundation.cimulink.core.records.ComponentPort;
import kotlin.Pair;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static com.verr1.controlcraft.foundation.cimulink.core.components.circuit.CircuitValidator.Assert;


public class CircuitDebugger {

    private final Circuit toDebug;

    private final Map<ComponentPortName, Integer> oiMap = new HashMap<>();
    private final Map<ComponentPortName, Integer> ioMap = new HashMap<>();

    private final List<ComponentPortName> trackedPort = new ArrayList<>();

    public CircuitDebugger(Circuit toDebug) {
        this.toDebug = toDebug;
        Assert(toDebug);
        var connectionIds = observeConnections();

        connectionIds.forEach(cid -> {
            oiMap.put(cid.cpo, cid.wireId());
            ioMap.put(cid.cpi, cid.wireId());
        });
    }




    public Set<ConnectionId> observeConnections(){
        int wireCount = toDebug.wireId2inputComponentPorts.size();
        return IntStream.range(0, wireCount)
                .boxed()
                .flatMap(i -> toDebug.wireId2inputComponentPorts
                        .get(i)
                        .stream()
                        .map(cpi -> new Pair<>(i,
                                new Pair<>(
                                        toDebug.wireId2outputComponentPort.get(i),
                                        cpi
                        )))
                )
                .map(wi_cpi_cpo -> new ConnectionId(
                        mapOutput(wi_cpi_cpo.getSecond().getFirst()),
                        mapInput(wi_cpi_cpo.getSecond().getSecond()),
                        wi_cpi_cpo.getFirst()
                ))
                .collect(Collectors.toSet());
    }

    public static void PrintOutputs(Circuit c){
        StringBuilder sb = new StringBuilder();
        c.outputs().forEach(
                o_name -> sb.append(o_name).append(": ").append(c.output(o_name)).append(" ")
        );
        System.out.println("Outputs: " + sb);
    }

    private int wireIdOf(ComponentPortName cp){

        if(ioMap.containsKey(cp)){
            return ioMap.get(cp);
        }

        if(oiMap.containsKey(cp)){
            return oiMap.get(cp);
        }
        return badPort(cp);
    }

    private int badPort(ComponentPortName cp){
        throw new IllegalArgumentException("cp: " + cp + " does not exist!, inputs: " + ioMap.keySet() + " outputs: " + oiMap.keySet());
    }

    public void printOutputs(){
        StringBuilder sb = new StringBuilder();
        toDebug.outputs().forEach(
                o_name -> sb.append(o_name).append(": ").append(toDebug.output(o_name)).append(" ")
        );
        System.out.println("Outputs: " + sb);
    }

    public void printOutputs(Function<Double, String> formatter, ComponentPortName... names){
        StringBuilder sb = new StringBuilder();
        for(var name: names){
            int wireId = wireIdOf(name);
            ComponentPort cp = toDebug.wireId2outputComponentPort.get(wireId);
            double out = toDebug.components.get(cp.componentId()).peekOutput(cp.portId());
            sb.append(name).append(" ").append(formatter.apply(out)).append("|");
        }
        System.out.println("Port Values: " + sb);
    }

    public void track(ComponentPortName... names){
        trackedPort.clear();
        for(var name: names){
            if(!ioMap.containsKey(name) && !oiMap.containsKey(name))badPort(name);
            trackedPort.add(name);
        }
    }

    public void printTracked(Function<Double, String> formatter){
        printOutputs(formatter, trackedPort.toArray(new ComponentPortName[0]));
    }

    public void printTracked(){
        printOutputs(d -> "" + d, trackedPort.toArray(new ComponentPortName[0]));
    }

    public static void PrintConnections(Collection<ConnectionId> connectionIds){
        System.out.println("Observed Connections: ");
        for (var c: connectionIds){
            System.out.println("out: " + c.cpo() + " in: " + c.cpi() + " wid: " + c.wireId);
        }
    }

    public static List<String> PrintConnectionsAsString(Collection<ConnectionId> connectionIds){
        List<String> connections = new ArrayList<>();
        for (var c: connectionIds){
            connections.add("out: " + c.cpo() + " in: " + c.cpi() + " wid: " + c.wireId);
        }
        return connections;
    }

    public void printConnections(){
        PrintConnections(observeConnections());
    }

    public List<String> printConnectionsAsString(){
        return PrintConnectionsAsString(observeConnections());
    }

    public void printPropagation(){
        StringBuilder sb = new StringBuilder();
        IntStream.range(0, toDebug.n()).mapToObj(i ->
            new Pair<>(toDebug.__in(i), toDebug.propagateTo(i).stream().map(toDebug::__out))
        ).forEach(p -> {
            sb.append(p.getFirst()).append("-->").append(p.getSecond().toList()).append("\n");
        });
        System.out.println(sb);
    }

    private ComponentPortName mapOutput(ComponentPort cp){
        NamedComponent nc = (NamedComponent)toDebug.components.get(cp.componentId());
        return new ComponentPortName(nc.name(), nc.outputs().get(cp.portId()));
    }

    private ComponentPortName mapInput(ComponentPort cp){
        NamedComponent nc = (NamedComponent)toDebug.components.get(cp.componentId());
        return new ComponentPortName(nc.name(), nc.inputs().get(cp.portId()));
    }

    public void trackWithPeriod(double ts, double observePeriod, double span){
        for (double t = 0; t < span; t += observePeriod) {

            for(int step = 0; step < observePeriod / ts; step++){
                toDebug.cycle();
            }

            System.out.printf("Time: %.4f%n", t);
            printTracked(d -> String.format("%.4f", d));
        }
    }


    public record ConnectionId(ComponentPortName cpo, ComponentPortName cpi, int wireId){}

}
