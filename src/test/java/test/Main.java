package test;

import cimulink.Circuit;
import cimulink.NamedComponent;
import cimulink.factory.NamedComponentFactory;
import cimulink.factory.preset.LinearAdderN;

import java.util.List;

public class Main {
    public static void main(String[] args) {
        System.out.println("Hello, Cimulink!");
        // Here you can add code to test your components or run simulations



    }

    public void fma_test(){
        LinearAdderN[] fma = new LinearAdderN[4];
        for(int i = 0; i < fma.length; i++){
            fma[i] = (LinearAdderN) NamedComponentFactory.LINEAR_FMA.apply(List.of(2.0, 1.0)).db_withName("fma_" + i);
        }

        Circuit.builder constructor = new Circuit.builder();

        for (LinearAdderN linearAdderN : fma) {
            constructor.addComponent(linearAdderN.db_name(), linearAdderN);
        }

        Circuit circuit = constructor
                .connect(fma[0].db_name(), fma[0].out(), fma[1].db_name(), fma[1].in(0))
                .connect(fma[0].db_name(), fma[0].out(), fma[2].db_name(), fma[2].in(0))
                .connect(fma[2].db_name(), fma[2].out(), fma[1].db_name(), fma[1].in(1))
                .connect(fma[3].db_name(), fma[3].out(), fma[2].db_name(), fma[2].in(1))
                .defineInput("i0", fma[0].db_name(), fma[0].in(0))
                .defineInput("i1", fma[0].db_name(), fma[0].in(1))
                .defineInput("i3", fma[3].db_name(), fma[3].in(0))
                .defineInput("i4", fma[3].db_name(), fma[3].in(1))
                .defineOutput("o", fma[1].db_name(), fma[1].out())
                .build();

        double out = circuit
                .input("i0", 1.0)
                .input("i1", 2.0)
                .input("i3", 3.0)
                .input("i4", 4.0)
                .forward()
                .output("o");
        ;
        System.out.println("circuit get: " + out);
    }

    public void test_0(){
        NamedComponent test_1 = new NamedComponent(
                new SimpleWireComponent(2, 1),
                List.of("a", "b"),
                List.of("o")
        ).db_withName("w1");

        NamedComponent test_2 = new NamedComponent(
                new SimpleWireComponent(2, 1),
                List.of("a", "b"),
                List.of("o")
        ).db_withName("w2");

        NamedComponent test_3 = new NamedComponent(
                new SimpleRegComponent(2, 1),
                List.of("a", "b"),
                List.of("o")
        ).db_withName("w3");

        Circuit testCircuit = new Circuit.builder()
                .addComponent("w1", test_1)
                .addComponent("w2", test_2)
                .addComponent("w3", test_3)
                .connect("w1", "o", "w2", "a")
                .connect("w2", "o", "w3", "a")
                .connect("w3", "o", "w1", "a")
                .defineInput("global_b", "w1", "b")
                .defineOutput("global_o", "w2","o")
                .build();
    }

}
