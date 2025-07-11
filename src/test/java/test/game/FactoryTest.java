package test.game;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.Circuit;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.CircuitDebugger;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitWorldBuilder;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.Summary;
import com.verr1.controlcraft.foundation.cimulink.game.debug.TestEnvBlockLinkWorld;
import com.verr1.controlcraft.foundation.cimulink.game.port.ISummarizable;
import com.verr1.controlcraft.foundation.cimulink.game.port.digital.*;
import com.verr1.controlcraft.foundation.cimulink.game.port.inout.InputLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.inout.OutputLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.FFTypes;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.GateTypes;
import com.verr1.controlcraft.foundation.cimulink.core.registry.CimulinkFactory;
import org.apache.commons.lang3.function.TriFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;

public class FactoryTest {

    public static void buildTag(){
        FMALinkPort fma = new FMALinkPort();
        InputLinkPort input = new InputLinkPort();
        OutputLinkPort output = new OutputLinkPort();
        ShifterLinkPort shifter = new ShifterLinkPort();

        fma.setName("fma");
        input.setName("global_i");
        output.setName("global_o");
        shifter.setName("shifter");

        TestEnvBlockLinkWorld.add(fma, input, output, shifter);

        fma.setCoefficients(List.of(0.0, 1.0));

        input.connectTo(input.out(0), fma.pos(), fma.in(0));
        fma.connectTo(fma.out(0), output.pos(), output.in(0));
        fma.connectTo(fma.out(0), shifter.pos(), shifter.in(0));
        shifter.connectTo(shifter.out(0), fma.pos(), fma.in(1));

        CircuitNbt nbt = CircuitWorldBuilder.of(List.of(fma, input, output, shifter)).buildNbt();

        Circuit circuit = (Circuit) nbt.buildCircuit().withName("circuit");

        CircuitDebugger debugger = new CircuitDebugger(circuit);

        debugger.printConnections();

    }

    public static void loadAndReload(){

        BiFunction<FFTypes, FFLinkPort, FFLinkPort> changeType = (t, ff) -> {ff.setCurrentType(t); return ff;};

        BiFunction<List<Double>, FMALinkPort, FMALinkPort> changeCoeff = (coeffs, fma) -> {
            fma.setCoefficients(coeffs);
            return fma;
        };

        BiFunction<GateTypes, GateLinkPort, GateLinkPort> changeGate = (t, gate) -> {
            gate.setCurrentType(t);
            return gate;
        };

        TriFunction<Integer, Integer, ShifterLinkPort, ShifterLinkPort> changeParallelDelay = (
                parallel,
                delay,
                shifter
        ) -> {
            shifter.setParallel(parallel);
            shifter.setDelay(delay);
            return shifter;
        };

        BiFunction<Boolean, ShifterLinkPort, ShifterLinkPort> changeAsync = (async, shifter) -> {
            shifter.setAsync(async);
            return shifter;
        };

        List<ISummarizable> components = List.of(
                new ComparatorLinkPort(),
                changeType.apply(FFTypes.D_FF, new FFLinkPort()),
                changeType.apply(FFTypes.T_FF, new FFLinkPort()),
                changeType.apply(FFTypes.RS_FF, new FFLinkPort()),
                changeType.apply(FFTypes.JK_FF, new FFLinkPort()),
                changeType.apply(FFTypes.ASYNC_D_FF, new FFLinkPort()),
                changeType.apply(FFTypes.ASYNC_T_FF, new FFLinkPort()),
                changeType.apply(FFTypes.ASYNC_JK_FF, new FFLinkPort()),
                changeType.apply(FFTypes.ASYNC_RS_FF, new FFLinkPort()),
                changeCoeff.apply(List.of(1.0), new FMALinkPort()),
                changeCoeff.apply(List.of(1.0, 2.0), new FMALinkPort()),
                changeCoeff.apply(List.of(1.0, 2.0, 3.0), new FMALinkPort()),
                changeGate.apply(GateTypes.AND, new GateLinkPort()),
                changeGate.apply(GateTypes.OR, new GateLinkPort()),
                changeGate.apply(GateTypes.XOR, new GateLinkPort()),
                changeGate.apply(GateTypes.NOT, new GateLinkPort()),
                new Mux2LinkPort(),
                changeAsync.apply(true, changeParallelDelay.apply(1, 1, new ShifterLinkPort())),
                changeAsync.apply(true, changeParallelDelay.apply(0, 1, new ShifterLinkPort())),
                changeAsync.apply(true, changeParallelDelay.apply(1, 2, new ShifterLinkPort())),
                changeAsync.apply(false, changeParallelDelay.apply(1, 1, new ShifterLinkPort())),
                changeAsync.apply(false, changeParallelDelay.apply(0, 1, new ShifterLinkPort())),
                changeAsync.apply(false, changeParallelDelay.apply(1, 2, new ShifterLinkPort()))
        );

        List<NamedComponent> restore = new ArrayList<>();

        components.forEach(c -> {
            Summary summary = c.summary();
            restore.add(CimulinkFactory.restore(summary, NamedComponent.class));
        });



    }

}
