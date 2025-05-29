package cimulink.factory;

import cimulink.factory.preset.Component21;
import cimulink.factory.preset.ComponentNM;
import cimulink.factory.preset.Schmitt;

import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

public class NamedComponentFactory {

    Factory21<Void> ADDER = () -> new Component21<>(
            inputs -> List.of(inputs.get(0) + inputs.get(1))
    );

    Factory21<Void> MUL = () -> new Component21<>(
            inputs -> List.of(inputs.get(0) * inputs.get(1))
    );

    BiFunction<Double, Double, Schmitt> SCHMITT = Schmitt::new;


    interface Factory21<S> extends Supplier<Component21<S>>{ }
    interface FactoryNM<S> extends Supplier<ComponentNM<S>>{ }

}
