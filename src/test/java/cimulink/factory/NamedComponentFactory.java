package cimulink.factory;

import cimulink.factory.basic.Component21;
import cimulink.factory.basic.ComponentNM;
import cimulink.factory.preset.LinearAdderN;
import cimulink.factory.preset.Schmitt;
import kotlin.Pair;

import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

public class NamedComponentFactory {

    public static Factory21<Void> ADDER = () -> new Component21<>(
            inputs -> List.of(inputs.get(0) + inputs.get(1))
    );

    public static Factory21<Void> MUL = () -> new Component21<>(
            inputs -> List.of(inputs.get(0) * inputs.get(1))
    );

    public static ContextFactory<Pair<Double, Double>, Schmitt> SCHMITT = ud -> new Schmitt(ud.getFirst(), ud.getSecond());

    public static ContextFactory<List<Double>, LinearAdderN> LINEAR_FMA = LinearAdderN::new;


    public interface Factory21<S> extends Supplier<Component21<S>>{ }
    public interface FactoryNM<S> extends Supplier<ComponentNM<S>>{ }
    public interface ContextFactory<C, S> extends Function<C, S>{}
}
