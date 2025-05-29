package cimulink.factory;

import java.util.List;
import java.util.function.Supplier;

public class NamedComponentFactory {

    Factory21<Void> ADDER = () -> new Component21<>(
            inputs -> List.of(inputs.get(0) + inputs.get(1))
    );






    interface Factory21<S> extends Supplier<Component21<S>>{ }
    interface FactoryNM<S> extends Supplier<ComponentNM<S>>{ }
}
