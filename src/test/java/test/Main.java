package test;

import cimulink.Circuit;
import cimulink.NamedComponent;
import cimulink.components.SimpleRegComponent;
import cimulink.components.SimpleWireComponent;

import java.util.List;

public class Main {
    public static void main(String[] args) {
        System.out.println("Hello, Cimulink!");
        // Here you can add code to test your components or run simulations

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
