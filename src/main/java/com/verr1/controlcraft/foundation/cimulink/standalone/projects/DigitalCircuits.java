package com.verr1.controlcraft.foundation.cimulink.standalone.projects;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.analog.AsyncShifter;
import com.verr1.controlcraft.foundation.cimulink.core.components.analog.Functions;
import com.verr1.controlcraft.foundation.cimulink.core.components.analog.LinearAdder;
import com.verr1.controlcraft.foundation.cimulink.core.components.analog.Shifter;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.Circuit;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.CircuitConstructor;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.CircuitDebugger;
import com.verr1.controlcraft.foundation.cimulink.core.components.digital.ff.FlipFlops;
import com.verr1.controlcraft.foundation.cimulink.core.components.digital.gates.Gates;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.ad.Comparator;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.da.Multiplexer;
import com.verr1.controlcraft.foundation.cimulink.core.components.sources.DirectCurrent;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import com.verr1.controlcraft.foundation.cimulink.standalone.CimulinkStandalone;

import java.nio.file.Path;
import java.util.List;

public class DigitalCircuits {

    public static CircuitNbt rangeAdder(){

        LinearAdder fma_0 = (LinearAdder)new LinearAdder(List.of(1.0, 1.0)).withName("FMA_0");
        Shifter reg_0 = (Shifter)new Shifter(0, 1).withName("REG_0");
        NamedComponent max = Functions.MAX.apply(2).withName("MAX_0");
        NamedComponent min = Functions.MIN.apply(2).withName("MIN_0");
        Multiplexer mux_0 = (Multiplexer)new Multiplexer(1).withName("MUX_0");

        CircuitConstructor constructor = new CircuitConstructor();

        constructor.addComponent(fma_0, reg_0, max, min, mux_0);

        constructor
                .defineInput("inc", mux_0.__dat(0))
                .defineInput("dec", mux_0.__dat(1))
                .defineInput("i/d", mux_0.__sel(0))
                .defineInput("max", min.__in(1))
                .defineInput("min", max.__in(1))
                .defineOutput("sum", reg_0.__out(0))

                .connect(mux_0.__out(0), fma_0.__in(1))
                .connect(fma_0.__out(0), max.__in(0))
                .connect(max.__out(0), min.__in(0))
                .connect(min.__out(0), reg_0.__in(0))
                .connect(reg_0.__out(0), fma_0.__in(0));

        return constructor.buildContext();

/*
        Circuit circuit = constructor.build("range_adder");
        CircuitDebugger debugger = new CircuitDebugger(circuit);
        debugger.track(circuit.__out(0), mux_0.__out(0), fma_0.__out(0), max.__out(0));
        circuit.input("inc", 1.0);
        circuit.input("dec", -1.0);
        circuit.input("max", 15.0);
        circuit.input("min", 0.0);

        circuit.input("i/d", 0.0);
        debugger.trackWithPeriod(0.05, 0.05, 1);
        circuit.input("i/d", 1.0);
        debugger.trackWithPeriod(0.05, 0.05, 1);
* */

    }

    public static CircuitNbt cycleAdder(){
        Gates.Gate or_clk = Gates.OR.apply(2);
        Gates.Gate or_sel = Gates.OR.apply(2);
        AsyncShifter reg = new AsyncShifter(0, 1);
        LinearAdder fma = new LinearAdder(List.of(1.0, 1.0));
        DirectCurrent inc = new DirectCurrent(1);
        Comparator cycle_comp = new Comparator();
        Multiplexer cycle_mux = new Multiplexer(1);

        CircuitConstructor constructor = new CircuitConstructor();
        constructor.addComponent(or_clk, or_sel, reg, fma, inc, cycle_comp, cycle_mux);

        constructor
                .defineInput("clk", or_clk.__in(0))
                .defineInput("rst", or_clk.__in(1), or_sel.__in(1))
                .defineInput("max", cycle_comp.__in(1)) // B
                .defineOutput("out", reg.__out(0))
                .connect(or_clk.__out(0), reg.__clk())
                .connect(inc.__out(0), fma.__in(1))
                .connect(reg.__out(0), fma.__in(0))
                .connect(fma.__out(0), cycle_mux.__dat(0), cycle_comp.__in(0)) // A
                .connect(cycle_comp.__ge(), or_sel.__in(0))
                .connect(or_sel.__out(0), cycle_mux.__sel(0))
                .connect(cycle_mux.__out(0), reg.__in(0));



        return constructor.buildContext();// constructor.buildContext();
    }

    public static CircuitNbt integralUnit(){
        Shifter reg = new Shifter(0, 1);
        Functions.FunctionN g_i = Functions.PRODUCT.apply(2);
        LinearAdder fma = new LinearAdder(1.0, 1.0);
        Functions.FunctionN i_min = Functions.MAX.apply(2);
        Functions.FunctionN i_max = Functions.MIN.apply(2);
        Multiplexer dat_rst = new Multiplexer(1);
        CircuitConstructor constructor = new CircuitConstructor();

        constructor.addComponent(reg, g_i, fma, i_min, i_max, dat_rst);
        constructor
                .defineInput("sample", g_i.__in(0))
                .defineInput("ts", g_i.__in(1))
                .defineInput("Imin", i_min.__in(1))
                .defineInput("Imax", i_max.__in(1))
                .defineInput("set", dat_rst.__dat(1))
                .defineInput("rst", dat_rst.__sel(0))
                .defineOutput("integral", i_max.__out(0))
                .connect(g_i.__out(0), fma.__in(0))
                .connect(fma.__out(0), i_min.__in(0))
                .connect(i_min.__out(0), i_max.__in(0))
                .connect(i_max.__out(0), dat_rst.__dat(0))
                .connect(dat_rst.__out(0), reg.__in(0))
                .connect(reg.__out(0), fma.__in(1));

        return constructor.buildContext();
    }

    public static CircuitNbt autoResetCycleAdder(){
        Circuit cycleAdder = cycleAdder().buildCircuit();
        Circuit integralUnit = integralUnit().buildCircuit();

        DirectCurrent ts = new DirectCurrent(1);
        DirectCurrent dec = new DirectCurrent(-1);

        DirectCurrent th = new DirectCurrent(0.5);
        Comparator cmp = new Comparator();
        NamedComponent tff = FlipFlops.T_FF.get();
        NamedComponent and = Gates.AND.apply(2);

        CircuitConstructor constructor = new CircuitConstructor();

        constructor.addComponent(cycleAdder, integralUnit, ts, dec, cmp, th, tff, and);

        constructor
                .defineInput("ticks", integralUnit.__in("set"), integralUnit.__in("Imax"))
                .defineInput("trigger", integralUnit.__in("rst"));

        return null;
    }

    public static void test(){
        Circuit circuit = integralUnit().buildCircuit();

        CircuitDebugger db = new CircuitDebugger(circuit);

        db.track(circuit.__out(0));

        circuit.input("sample", 1).input("ts", 1).input("Imin", 0).input("Imax", 10).input("set", 2);

        for (int i = 0; i < 10; i++) {
            db.printTracked();
            circuit.cycle();
        }
        System.out.println("------cycle 0");
        circuit.input("rst", 1);
        circuit.cycle();
        circuit.input("rst", 0);
        System.out.println("------cycle 1");
        for (int i = 0; i < 10; i++) {
            db.printTracked();
            circuit.cycle();
        }
    }

    public static void testCycleAdder(){
        Circuit circuit = cycleAdder().buildCircuit();

        CircuitDebugger db = new CircuitDebugger(circuit);

        db.track(circuit.__out(0));

        circuit.input("max", 5);

        for (int i = 0; i < 8; i++) {
            circuit.input("clk", 1);
            circuit.cycle();
            circuit.input("clk", 0);
            circuit.cycle();
            db.printTracked();
        }

        circuit.input("rst", 1);
        circuit.cycle();
        db.printTracked();
    }

}
