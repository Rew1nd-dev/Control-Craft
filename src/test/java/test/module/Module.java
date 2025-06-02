package test.module;

import cimulink.v3.components.analog.Shifter;
import cimulink.v3.components.circuit.Circuit;
import cimulink.v3.components.circuit.CircuitConstructor;
import cimulink.v3.components.circuit.CircuitDebugger;
import cimulink.v3.components.general.ad.ADConverter;
import cimulink.v3.components.general.ad.Comparator;
import cimulink.v3.components.general.da.Multiplexer;
import cimulink.v3.components.sources.DirectCurrent;
import cimulink.v3.records.ComponentPortName;

import java.util.Arrays;

import static cimulink.v3.components.circuit.CircuitDebugger.PrintConnections;
import static cimulink.v3.components.circuit.CircuitDebugger.PrintOutputs;

public class Module {

    public static void comparatorTest(){
        Comparator cmp = (Comparator)new Comparator().withName("cmp");

        CircuitConstructor constructor = new CircuitConstructor();
        constructor.addComponent(cmp)
                .defineInput("a", cmp.__a())
                .defineInput("b", cmp.__b())
                .defineOutput("ge", cmp.__ge())
                .defineOutput("le", cmp.__le())
                .defineOutput("eq", cmp.__eq());

        Circuit c = constructor.build("circuit");

        c.input("a", 1.0).input("b", 0.0);
        c.cycle();
        PrintOutputs(c);
        c.input("a", 0.0).input("b", 0.0);
        c.cycle();
        PrintOutputs(c);
        c.input("a", 0.0).input("b", 1.0);
        c.cycle();
        PrintOutputs(c);


    }

    public static void adConverterTest(){
        int n = 3;
        ADConverter adc = (ADConverter)new ADConverter(n, 2.0).withName("adc");
        CircuitConstructor constructor = new CircuitConstructor();
        constructor.addComponent(adc);

        for(int i = 0; i < n; i++){
            constructor.defineInput("i" + i, adc.__in(i));
            constructor.defineOutput("o" + i, adc.__out(i));
        }

        Circuit c = constructor.build("test");

        c.input("i0", 0.5).input("i1", 1.5).input("i2", 2.5);
        c.cycle();
        StringBuilder sb;

        sb = new StringBuilder();
        for(int i = 0; i < n; i++){
            sb.append("o").append(i).append(": ").append(c.output("o" + i)).append("\n");
        }

        System.out.println(sb);

        c.input("i0", 3.5).input("i1", 0.5).input("i2", 2.5);
        c.cycle();

        PrintOutputs(c);

    }

    public static void dcTest(){
        DirectCurrent dc = (DirectCurrent) new DirectCurrent(5.0).withName("dc");
        CircuitConstructor constructor = new CircuitConstructor();
        constructor.addComponent(dc);

        constructor.defineOutput("o", dc.__out(0));
                //.defineInput("i", dc.__in(0));

        Circuit c = constructor.build("dc_test");

        c.cycle();

        PrintOutputs(c);
    }

    public static void multiplexerTest(){
        int n = 2;
        Multiplexer mux = (Multiplexer)new Multiplexer(n).withName("mux");

        CircuitConstructor constructor = new CircuitConstructor();
        constructor.addComponent(mux);

        for(int i = 0; i < n; i++){
            constructor.defineInput("sel" + i, mux.__sel(i));
        }

        for(int i = 0; i < (1 << n); i++){
            constructor.defineInput("dat" + i, mux.__dat(i));
        }

        constructor.defineOutput("o", mux.__out(0));

        Circuit c = constructor.build("test");
        c.input("dat0", 0.0).input("dat1", 1.0).input("dat2", 2.0).input("dat3", 3.0);

        c.input("sel0", 0.0).input("sel1", 0.0);
        c.cycle();
        PrintOutputs(c);
        c.input("sel0", 1.0).input("sel1", 0.0);
        c.cycle();
        PrintOutputs(c);

        c.input("sel0", 0.0).input("sel1", 1.0);
        c.cycle();
        PrintOutputs(c);

        c.input("sel0", 1.0).input("sel1", 1.0);
        c.cycle();
        PrintOutputs(c);
    }

    public static void shifterTest(){
        Shifter reg = (Shifter) new Shifter(1, 1).withName("reg");
        CircuitConstructor constructor = new CircuitConstructor();
        constructor
                .addComponent(reg)
                .defineInput("i", reg.__in(0))
                .defineOutput("o", reg.__out(0))
        ;

        Circuit c = constructor.build("circuit");

        CircuitDebugger debugger = new CircuitDebugger(c);
        PrintConnections(debugger.observeConnections());

        System.out.println(c.output("o"));

        PrintOutputs(c);
    }

    public static void shifterSerialTest(){
        int n = 4;
        Shifter[] reg = new Shifter[n];

        CircuitConstructor constructor = new CircuitConstructor();
        for(int i = 0; i < n; i++){
            reg[i] = (Shifter) new Shifter(0, 1).withName("reg_" + i);
        }
        constructor.addComponent(reg);
        for(int i = 0; i < n - 1; i++){
            constructor.connect(reg[i].__out(0), reg[i + 1].__in(0));
        }
        constructor
                .defineInput("i", reg[0].__in(0))
                .defineOutput("o", reg[n - 1].__out(0));

        Circuit c = constructor.build("circuit");


        CircuitDebugger debugger = new CircuitDebugger(c);
        debugger.track(Arrays.stream(reg).map(r -> r.__out(0)).toList().toArray(new ComponentPortName[0]));

        for(int i = 0; i < 15; i++){
            c.input("i", i);

            c.cycle();
            debugger.printTracked();
        }


    }

}
