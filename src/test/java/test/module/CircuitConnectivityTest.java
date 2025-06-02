package test.module;

import cimulink.v3.components.analog.Shifter;
import cimulink.v3.components.circuit.Circuit;
import cimulink.v3.components.analog.LinearAdder;
import cimulink.v3.components.circuit.CircuitConstructor;
import cimulink.v3.components.circuit.CircuitDebugger;
import cimulink.v3.components.general.ad.Comparator;
import cimulink.v3.components.general.da.Multiplexer;
import cimulink.v3.components.sources.DirectCurrent;

import java.util.List;

import static cimulink.v3.components.circuit.CircuitDebugger.PrintConnections;
import static test.module.Packaging.MAX;

public class CircuitConnectivityTest {

    public static void invalidLoopTest(){
        LinearAdder fma_0 = (LinearAdder)new LinearAdder(List.of(1.0, 1.0)).withName("fma_0");
        LinearAdder fma_1 = (LinearAdder)new LinearAdder(List.of(1.0, 1.0)).withName("fma_1");

        CircuitConstructor constructor = new CircuitConstructor();
        constructor.addComponent(fma_0, fma_1);
        constructor
                .connect(fma_0.__out(0), fma_1.__in(0))
                .connect(fma_1.__out(0), fma_0.__in(0));
        constructor.build();

    }




    public static void monostableTest(){
        LinearAdder fma = (LinearAdder)new LinearAdder(List.of(-1.0, 1.0)).withName("fma_0");
        Shifter reg = (Shifter) new Shifter(0, 1).withName("reg");
        Multiplexer mux_2 = (Multiplexer) new Multiplexer(1).withName("mux_2");
        Comparator cmp2 = (Comparator)new Comparator().withName("cmp2");
        DirectCurrent dc1 = (DirectCurrent)new DirectCurrent(1).withName("dc1");

        CircuitConstructor constructor = new CircuitConstructor();
        constructor.addComponent(fma, reg, mux_2, cmp2, dc1);
        constructor
                .connect(cmp2.__le(), mux_2.__sel(0))
                .connect(fma.__out(0), mux_2.__dat(0))
                .connect(fma.__out(0), cmp2.__a())
                .defineInput("i", mux_2.__dat(1))
                .defineInput("i", cmp2.__b())
                .connect(mux_2.__out(0), reg.__in(0))
                .connect(reg.__out(0), fma.__in(1))
                .connect(dc1.__out(0), fma.__in(0))
                .defineOutput("o", fma.__out(0));

        Circuit c = constructor.build("circuit");

        CircuitDebugger dbg = new CircuitDebugger(c);
        dbg.printConnections();
        dbg.track(reg.__in(0), fma.__out(0), fma.__in(0), fma.__in(1), mux_2.__out(0), c.__out(0));
        dbg.printPropagation();

        c.input("i", 15);
        c.cycle();
        dbg.printTracked();
        c.input("i", 0);
        for (int i = 0; i < 20; i++) {
            c.cycle();
            dbg.printTracked();
        }

        System.out.println("input changed");

        c.input("i", 15.0);
        c.cycle();
        c.input("i", 0);
        for (int i = 0; i < 20; i++) {
            c.cycle();
            dbg.printTracked();
        }
    }

    public static void propagateMapTest(){

        CircuitConstructor constructor = new CircuitConstructor();

        LinearAdder[] fma = new LinearAdder[4];
        Shifter[] reg = new Shifter[2];
        for (int i = 0; i < fma.length; i++){
            fma[i] = (LinearAdder) new LinearAdder(List.of(1.0, 1.0)).withName("A_" + i);
            constructor.addComponent(fma[i].name(), fma[i]);
        }
        for (int i = 0; i < reg.length; i++){
            reg[i] = (Shifter) new Shifter(0, 1).withName("R_" + i);
            constructor.addComponent(reg[i].name(), reg[i]);
        }
        constructor
                .defineInput("i0", fma[0].__in(0))
                .defineInput("i1", fma[1].__in(0))
                .defineInput("i1", fma[0].__in(1))
                .defineInput("i2", fma[1].__in(1))
                .defineInput("i2", reg[0].__in(0))
                .defineInput("i2", reg[1].__in(0))
                .connect(fma[0].__out(0), fma[2].__in(1))
                .connect(fma[1].__out(0), fma[3].__in(0))
                .connect(reg[1].__out(0), fma[3].__in(1))
                .connect(reg[0].__out(0), fma[2].__in(0))
                .defineOutput("o_reg0", reg[0].__out(0))
                .defineOutput("fma2o", fma[2].__out(0))
                .defineOutput("fma3o", fma[3].__out(0));

        Circuit c = constructor.build("circuit");
        CircuitDebugger debugger = new CircuitDebugger(c);
        debugger.printPropagation();


    }




    public static void validLoopTest(){
        LinearAdder fma = (LinearAdder)new LinearAdder(List.of(1.0, 1.0)).withName("fma");
        Shifter reg = (Shifter) new Shifter(0, 1).withName("reg");

        CircuitConstructor constructor = new CircuitConstructor();
        constructor.addComponent(fma, reg);
        constructor
                .connect(fma.__out(0), reg.__in(0))
                .connect(reg.__out(0), fma.__in(1))
                .defineInput("i", fma.__in(0))
                .defineOutput("o", fma.__out(0));
        Circuit c = constructor.build("circuit");

        CircuitDebugger debugger = new CircuitDebugger(c);
        debugger.printConnections();

        System.out.println(c.output("o"));

        for(int i = 0; i < 10; i++){
            c.input("i", 1);
            c.cycle();
            System.out.println(c.output("o"));
        }

    }

    public static void serialTest(){

        LinearAdder[] fma = new LinearAdder[4];
        CircuitConstructor constructor = new CircuitConstructor();
        for (int i = 0; i < fma.length; i++){
            fma[i] = (LinearAdder) new LinearAdder(List.of(1.0, 1.0)).withName("A_" + i);
            constructor.addComponent(fma[i].name(), fma[i]);
        }


        Circuit c = constructor
            .connect(fma[0].__out(0), fma[1].__in(1))
            .connect(fma[0].__out(0), fma[2].__in(0))
            .connect(fma[1].__out(0), fma[3].__in(0))
            .connect(fma[2].__out(0), fma[3].__in(1))
            .defineInput("_i0", fma[0].__in(0))
            .defineInput("_i1", fma[0].__in(1))
            .defineInput("_i2", fma[1].__in(0))
            .defineInput("_i4", fma[2].__in(1))
            .defineOutput("_o", fma[3].__out(0))
            .build("circuit");



        CircuitDebugger debugger = new CircuitDebugger(c);

        c.input(c.namedInputs().get("_i0"), 1);
        c.input(c.namedInputs().get("_i1"), 2);
        c.input(c.namedInputs().get("_i2"), 3);
        c.input(c.namedInputs().get("_i4"), 4);
        c.onInputChange(0, 1, 2, 3);

        PrintConnections(debugger.observeConnections());

        System.out.println(c.retrieveOutput());
    }

}
