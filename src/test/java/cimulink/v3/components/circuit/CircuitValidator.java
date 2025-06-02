package cimulink.v3.components.circuit;

import cimulink.v3.records.ComponentPort;
import cimulink.v3.utils.ArrayUtils;

import java.util.Collection;

public class CircuitValidator {

    public static void Assert(Circuit circuit){
        AssertWireIdConsistency(circuit);
        AssertComponentMappings(circuit);
    }

    public static void AssertWireIdConsistency(Circuit circuit){
        ArrayUtils.AssertSameSize(circuit.wireId2inputComponentPorts, circuit.wireId2outputComponentPort);
    }


    public static void AssertComponentMappings(Circuit circuit){
        circuit.wireId2inputComponentPorts.stream().flatMap(Collection::stream).forEach(componentPort -> AssertValidInput(circuit, componentPort));
        circuit.wireId2outputComponentPort.forEach(componentPort -> AssertValidOutput(circuit, componentPort));
    }

    public static void AssertValidInput(Circuit circuit, ComponentPort cp){
        int cid = cp.componentId();
        int pid = cp.portId();
        int componentSize = circuit.components.size();
        ArrayUtils.AssertRange(cid, componentSize);
        ArrayUtils.AssertRange(pid, circuit.components.get(cid).n());
    }

    public static void AssertValidOutput(Circuit circuit, ComponentPort cp){
        int cid = cp.componentId();
        int pid = cp.portId();
        int componentSize = circuit.components.size();
        ArrayUtils.AssertRange(cid, componentSize);
        ArrayUtils.AssertRange(pid, circuit.components.get(cid).m());
    }

}
