package test.module;

import cimulink.v3.components.analog.LinearAdder;
import cimulink.v3.components.analog.Shifter;
import cimulink.v3.components.circuit.Circuit;
import cimulink.v3.components.circuit.CircuitConstructor;
import cimulink.v3.components.circuit.CircuitDebugger;
import cimulink.v3.components.general.ad.Comparator;
import cimulink.v3.components.general.da.Multiplexer;

import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

public class Packaging {

    public static final Supplier<Circuit> MAX = () -> {
        Comparator cmp = (Comparator) new Comparator().withName("cmp");
        Multiplexer mux_2 = (Multiplexer) new Multiplexer(1).withName("mux_2");

        CircuitConstructor constructor = new CircuitConstructor();
        constructor.addComponent(cmp, mux_2);
        constructor
                .defineInput("A", cmp.__a())
                .defineInput("B", cmp.__b())
                .defineInput("A", mux_2.__dat(0))
                .defineInput("B", mux_2.__dat(1))
                .defineOutput("max", mux_2.__out(0))
                .connect(cmp.__le(), mux_2.__sel(0));

        return constructor.build();
    };

    public static final Supplier<Circuit> MIN = () -> {
        Comparator cmp = (Comparator) new Comparator().withName("cmp");
        Multiplexer mux_2 = (Multiplexer) new Multiplexer(1).withName("mux_2");

        CircuitConstructor constructor = new CircuitConstructor();
        constructor.addComponent(cmp, mux_2);
        constructor
                .defineInput("A", cmp.__a())
                .defineInput("B", cmp.__b())
                .defineInput("A", mux_2.__dat(0))
                .defineInput("B", mux_2.__dat(1))
                .defineOutput("min", mux_2.__out(0))
                .connect(cmp.__le(), mux_2.__sel(0));

        return constructor.build();
    };

    public static final Supplier<Circuit> ACCUMULATOR = () -> {
        LinearAdder fma = (LinearAdder)new LinearAdder(List.of(1.0, 1.0)).withName("adder");
        Shifter reg = (Shifter) new Shifter(0, 1).withName("reg");


        CircuitConstructor constructor = new CircuitConstructor();
        constructor.addComponent(fma, reg);
        constructor
                .defineInput("i", fma.__in(0))
                .defineOutput("sum", fma.__out(0))
                .connect(fma.__out(0), reg.__in(0))
                .connect(reg.__out(0), fma.__in(1));


        return constructor.build();
    };

    public static final Supplier<Circuit> DIFFERENTIAL = () -> {
        LinearAdder fma = (LinearAdder)new LinearAdder(List.of(-1.0, 1.0)).withName("adder");
        Shifter reg = (Shifter) new Shifter(0, 1).withName("reg");


        CircuitConstructor constructor = new CircuitConstructor();
        constructor.addComponent(fma, reg);
        constructor
                .defineInput("i", fma.__in(1))
                .defineInput("i", reg.__in(0))
                .defineOutput("diff", fma.__out(0))
                .connect(reg.__out(0), fma.__in(0));


        return constructor.build();
    };

    // y = y' - u
    public static final Function<Double, Circuit> ORDINAL_1 = (ts) -> {
        LinearAdder fma_0 = (LinearAdder)new LinearAdder(List.of(ts, 1.0 + ts)).withName("fma_0");
        Shifter reg_0 = (Shifter) new Shifter(0, 1).withName("reg_0");

        CircuitConstructor constructor = new CircuitConstructor();
        constructor.addComponent(fma_0, reg_0);
        constructor
                .defineInput("i", fma_0.__in(0))
                .defineOutput("o", fma_0.__out(0))
                .connect(fma_0.__out(0), reg_0.__in(0))
                .connect(reg_0.__out(0), fma_0.__in(1));


        return constructor.build();
    };

    // y = y'' - u
    public static final Function<Double, Circuit> ORDINAL_2 = (ts) -> {
        LinearAdder fma_0 = (LinearAdder)new LinearAdder(List.of(ts * ts, 2.0, ts * ts - 1.0)).withName("fma_0");
        Shifter reg_0 = (Shifter) new Shifter(0, 1).withName("reg_0");
        Shifter reg_1 = (Shifter) new Shifter(0, 1).withName("reg_1");

        CircuitConstructor constructor = new CircuitConstructor();
        constructor.addComponent(fma_0, reg_0, reg_1);
        constructor
                .defineInput("i", fma_0.__in(0))
                .defineOutput("o", fma_0.__out(0))
                .connect(fma_0.__out(0), reg_0.__in(0))
                .connect(reg_0.__out(0), reg_1.__in(0))
                .connect(reg_1.__out(0), fma_0.__in(2))
                .connect(reg_0.__out(0), fma_0.__in(1));

        return constructor.build();
    };



    public static void packageTest_0(){

        Circuit max_0 = (Circuit) MAX.get().withName("max_0");
        Circuit max_1 = (Circuit) MAX.get().withName("max_1");

        CircuitConstructor constructor = new CircuitConstructor();
        constructor.addComponent(max_0, max_1);
        constructor
                .connect(max_0.__out(0), max_1.__in(0))
                .defineInput("A", max_0.__in(0))
                .defineInput("B", max_1.__in(1))
                .defineInput("C", max_0.__in(1))
                .defineOutput("max", max_1.__out(0));

        Circuit c = constructor.build("circuit");
        CircuitDebugger debugger = new CircuitDebugger(c);

        c.input("A", 1.0).input("B", 2.0).input("C", 3.0);
        c.cycle();
        debugger.printPropagation();
        debugger.printOutputs();
    }

    public static void packageTest_1(){

        double p = 15, i = 1, d = 4;
        double ts = 1.0 / 100.0;

        Circuit integral = (Circuit) ACCUMULATOR.get().withName("sum");
        Circuit differential = (Circuit) DIFFERENTIAL.get().withName("diff");
        LinearAdder proportional = (LinearAdder)new LinearAdder(List.of(1.0)).withName("proportional");
        LinearAdder err = (LinearAdder)new LinearAdder(List.of(1.0, -1.0)).withName("fma");

        Shifter reg = (Shifter) new Shifter(0, 1).withName("reg");

        LinearAdder add = (LinearAdder)new LinearAdder(List.of(p, i * ts, d * 1 / ts)).withName("add");

        Circuit plant = (Circuit) ORDINAL_2.apply(ts).withName("plant");

        CircuitConstructor closeLoop = new CircuitConstructor();
        closeLoop.addComponent(integral, differential, proportional, err, add, plant, reg);
        closeLoop
                .defineInput("target", err.__in(0))
                .defineOutput("output", plant.__out(0))
                .connect(err.__out(0), reg.__in(0))
                .connect(reg.__out(0), proportional.__in(0))
                .connect(reg.__out(0), integral.__in(0))
                .connect(reg.__out(0), differential.__in(0))
                .connect(proportional.__out(0), add.__in(0))
                .connect(integral.__out(0), add.__in(1))
                .connect(differential.__out(0), add.__in(2))
                .connect(add.__out(0), plant.__in(0))
                .connect(plant.__out(0), err.__in(1));

        Circuit close = closeLoop.build("close_loop");

        CircuitDebugger debugger = new CircuitDebugger(close);
        debugger.printConnections();
        debugger.track(
                plant.__out(0),
                err.__out(0),
                plant.__in(0)
        );

        close.input("target", 4.0);
        debugger.trackWithPeriod(ts, 0.2, 20.0);
    }

}
