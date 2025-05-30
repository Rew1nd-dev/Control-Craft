package cimulink.v1.factory;

import cimulink.v1.factory.basic.analog.Analog21;
import cimulink.v1.factory.basic.analog.AnalogNM;
import cimulink.v1.factory.preset.analog.LinearAdderN;
import cimulink.v1.factory.preset.analog.Schmitt;
import kotlin.Pair;

import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

public class NamedComponentFactory {

    public static Factory21<Void> ADDER = () -> new Analog21<>(
            inputs -> List.of(inputs.get(0) + inputs.get(1))
    );

    public static Factory21<Void> MUL = () -> new Analog21<>(
            inputs -> List.of(inputs.get(0) * inputs.get(1))
    );

    public static ContextFactory<Pair<Double, Double>, Schmitt> SCHMITT = ud -> new Schmitt(ud.getFirst(), ud.getSecond());

    public static ContextFactory<List<Double>, LinearAdderN> LINEAR_FMA = LinearAdderN::new;


    public interface Factory21<S> extends Supplier<Analog21<S>>{ }
    public interface FactoryNM<S> extends Supplier<AnalogNM<S>>{ }
    public interface ContextFactory<C, S> extends Function<C, S>{}
}
