package test.module;

import cimulink.Circuit;
import cimulink.factory.basic.digital.DigitalNM;
import cimulink.factory.preset.analog.ShifterNM;
import cimulink.factory.preset.digital.ff.DFlipFlop;
import cimulink.factory.preset.digital.ff.TFlipFlop;

import java.util.List;

public class FFTest {

    public static void shifterTest(){
        ShifterNM shifterNM = (ShifterNM) new ShifterNM(4, 3).db_withName("s1");

        Circuit circuit = new Circuit.builder()
                .addComponent(shifterNM.db_name(), shifterNM)
                .defineInput("i0", shifterNM.db_name(), shifterNM.in(0))
                .defineInput("i1", shifterNM.db_name(), shifterNM.in(1))
                .defineInput("i2", shifterNM.db_name(), shifterNM.in(2))
                .defineOutput("o0", shifterNM.db_name(), shifterNM.out(0))
                .defineOutput("o1", shifterNM.db_name(), shifterNM.out(1))
                .defineOutput("o2", shifterNM.db_name(), shifterNM.out(2))
                .build();

        /*
        circuit.input("i0", DigitalNM.TRUE).input("i1", DigitalNM.FALSE).input("i2", DigitalNM.TRUE);
        for(int i = 0; i < 6; i++){
            circuit.forward();
            System.out.println(getOutput(circuit, List.of("o0", "o1", "o2")));
        }
        System.out.println("Input Changed");
        circuit.input("i0", DigitalNM.FALSE).input("i1", DigitalNM.TRUE).input("i2", DigitalNM.FALSE);
        for(int i = 0; i < 6; i++){
            circuit.forward();
            System.out.println(getOutput(circuit, List.of("o0", "o1", "o2")));
        }
        System.out.println("Input Changed");
        * */

        circuit.input("i0", DigitalNM.TRUE).input("i1", DigitalNM.TRUE).input("i2", DigitalNM.TRUE);
        circuit.forward();
        System.out.println(getOutput(circuit, List.of("o0", "o1", "o2")));
        circuit.input("i0", DigitalNM.FALSE).input("i1", DigitalNM.FALSE).input("i2", DigitalNM.TRUE);
        circuit.forward();
        System.out.println(getOutput(circuit, List.of("o0", "o1", "o2")));
        circuit.input("i0", DigitalNM.FALSE).input("i1", DigitalNM.TRUE).input("i2", DigitalNM.FALSE);
        circuit.forward();
        System.out.println(getOutput(circuit, List.of("o0", "o1", "o2")));
        circuit.input("i0", DigitalNM.FALSE).input("i1", DigitalNM.TRUE).input("i2", DigitalNM.TRUE);
        circuit.forward();
        System.out.println(getOutput(circuit, List.of("o0", "o1", "o2")));
        circuit.forward();
        System.out.println(getOutput(circuit, List.of("o0", "o1", "o2")));
        circuit.forward();
        System.out.println(getOutput(circuit, List.of("o0", "o1", "o2")));
        circuit.forward();
        System.out.println(getOutput(circuit, List.of("o0", "o1", "o2")));
    }

    public static void dffTest(){
        DFlipFlop dff = new DFlipFlop();
        Circuit circuit = new Circuit.builder()
                .addComponent(dff.db_name(), dff)
                .defineInput("i", dff.db_name(), dff.i())
                .defineOutput("o", dff.db_name(), dff.o())
                .build();


        circuit.input("i", DigitalNM.TRUE);
        circuit.forward();
        System.out.println(circuit.output("o"));
        circuit.input("i", DigitalNM.FALSE);
        circuit.forward();
        System.out.println(circuit.output("o"));
        circuit.input("i", DigitalNM.TRUE);
        circuit.forward();
        System.out.println(circuit.output("o"));
    }

    public static void tffTest(){
        TFlipFlop tff = new TFlipFlop();
        Circuit circuit = new Circuit.builder()
                .addComponent(tff.db_name(), tff)
                .defineInput("i", tff.db_name(), tff.i())
                .defineOutput("o", tff.db_name(), tff.o())
                .build();


        circuit.input("i", DigitalNM.TRUE);
        circuit.forward();
        System.out.println(circuit.output("o"));
        circuit.input("i", DigitalNM.FALSE);
        circuit.forward();
        System.out.println(circuit.output("o"));
        circuit.input("i", DigitalNM.TRUE);
        circuit.forward();
        System.out.println(circuit.output("o"));
    }


    private static List<Boolean> getOutput(Circuit circuit, List<String> outputNames){
        return outputNames.stream().map(circuit::output).map(d -> d > 0.5).toList();
    }

}
