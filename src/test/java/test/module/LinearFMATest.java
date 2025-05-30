package test.module;

import cimulink.v1.Circuit;
import cimulink.v1.factory.NamedComponentFactory;
import cimulink.v1.factory.preset.analog.LinearAdderN;

import java.util.List;

public class LinearFMATest {
    public void test(){
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
}
