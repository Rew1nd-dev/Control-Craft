package com.verr1.controlcraft.foundation.cimulink.core.registry;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.analog.AsyncShifter;
import com.verr1.controlcraft.foundation.cimulink.core.components.analog.Functions;
import com.verr1.controlcraft.foundation.cimulink.core.components.analog.LinearAdder;
import com.verr1.controlcraft.foundation.cimulink.core.components.analog.Shifter;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.Circuit;
import com.verr1.controlcraft.foundation.cimulink.core.components.digital.ff.*;
import com.verr1.controlcraft.foundation.cimulink.core.components.digital.gates.FlexibleGate;
import com.verr1.controlcraft.foundation.cimulink.core.components.digital.gates.Gates;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.ad.Comparator;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.da.Multiplexer;
import com.verr1.controlcraft.foundation.cimulink.core.components.sources.DirectCurrent;
import com.verr1.controlcraft.foundation.cimulink.core.components.vectors.Cross;
import com.verr1.controlcraft.foundation.cimulink.core.components.vectors.Dot;
import com.verr1.controlcraft.foundation.cimulink.core.components.vectors.QTransform;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.Summary;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import net.minecraft.nbt.CompoundTag;
import org.apache.commons.lang3.NotImplementedException;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;

public class CimulinkFactory {

    public static final Map<String, Factory<?>> REGISTRY = new HashMap<>();

    private static final String PREFIX = "cimulink:";

    public static final Factory<Circuit> CIRCUIT = register(
            SerializeUtils.of(
                    Circuit::serialize,
                    Circuit::deserialize
            ),
            Circuit.class,
            defaultID("circuit")
    );

    public static final Factory<DirectCurrent> DC = register(
            SerializeUtils.of(
                    DirectCurrent::serialize,
                    DirectCurrent::deserialize
            ),
            DirectCurrent.class,
            defaultID("direct_current")
    );

    public static final Factory<Comparator> COMPARATOR = register(
            createParamLess(Comparator::new),
            Comparator.class,
            defaultID("comparator")
    );

    public static final Factory<NamedComponent> D_FF = register(
            createParamLess(FlipFlops.D_FF::get),
            NamedComponent.class,
            defaultID("dff")
    );

    public static final Factory<NamedComponent> T_FF = register(
            createParamLess(FlipFlops.T_FF::get),
            NamedComponent.class,
            defaultID("tff")
    );

    public static final Factory<NamedComponent> JK_FF = register(
            createParamLess(FlipFlops.JK_FF::get),
            NamedComponent.class,
            defaultID("jkff")
    );

    public static final Factory<NamedComponent> RS_FF = register(
            createParamLess(FlipFlops.RS_FF::get),
            NamedComponent.class,
            defaultID("rsff")
    );

    public static final Factory<AsyncDFlipFlop> ASYNC_D_FF = register(
            createParamLess(AsyncDFlipFlop::new),
            AsyncDFlipFlop.class,
            defaultID("async_dff")
    );

    public static final Factory<AsyncTFlipFlop> ASYNC_T_FF = register(
            createParamLess(AsyncTFlipFlop::new),
            AsyncTFlipFlop.class,
            defaultID("async_tff")
    );

    public static final Factory<AsyncRSFlipFlop> ASYNC_RS_FF = register(
            createParamLess(AsyncRSFlipFlop::new),
            AsyncRSFlipFlop.class,
            defaultID("async_rsff")
    );

    public static final Factory<AsyncJKFlipFlop> ASYNC_JK_FF = register(
            createParamLess(AsyncJKFlipFlop::new),
            AsyncJKFlipFlop.class,
            defaultID("async_jkff")
    );

    public static final Factory<LinearAdder> FMA = register(
            SerializeUtils.of(
                    LinearAdder::serialize,
                    LinearAdder::deserialize
            ),
            LinearAdder.class,
            defaultID("fma")
    );

    public static final Factory<Functions.FunctionN> PRODUCT = register(
            SerializeUtils.of(
                    Functions.FunctionN::serialize,
                    t -> Functions.PRODUCT.apply(Functions.deserializeN(t))
            ),
            Functions.FunctionN.class,
            defaultID("product")
    );

    public static final Factory<Functions.FunctionN> DIV = register(
            SerializeUtils.of(
                    Functions.FunctionN::serialize,
                    t -> Functions.DIV.get()
            ),
            Functions.FunctionN.class,
            defaultID("divide")
    );

    public static final Factory<Functions.FunctionN> MAX = register(
            SerializeUtils.of(
                    Functions.FunctionN::serialize,
                    t -> Functions.MAX.apply(Functions.deserializeN(t))
            ),
            Functions.FunctionN.class,
            defaultID("max")
    );

    public static final Factory<Functions.FunctionN> MIN = register(
            SerializeUtils.of(
                    Functions.FunctionN::serialize,
                    t -> Functions.MIN.apply(Functions.deserializeN(t))
            ),
            Functions.FunctionN.class,
            defaultID("min")
    );


    public static final Factory<Functions.FunctionN> ANGLE_FIX = register(
            SerializeUtils.of(
                    Functions.FunctionN::serialize,
                    t -> Functions.ANGLE_FIX.get()
            ),
            Functions.FunctionN.class,
            defaultID("angle_fix")
    );

    public static final Factory<Functions.FunctionN> POWER = register(
            SerializeUtils.of(
                    Functions.FunctionN::serialize,
                    t -> Functions.POWER.get()
            ),
            Functions.FunctionN.class,
            defaultID("power")
    );

    public static final Factory<Functions.FunctionN> ABS = register(
            SerializeUtils.of(
                    Functions.FunctionN::serialize,
                    t -> Functions.ABS.get()
            ),
            Functions.FunctionN.class,
            defaultID("abs")
    );

    public static final Factory<Functions.FunctionN> SIN = register(
            SerializeUtils.of(
                    Functions.FunctionN::serialize,
                    t -> Functions.SIN.get()
            ),
            Functions.FunctionN.class,
            defaultID("sin")
    );

    public static final Factory<Functions.FunctionN> COS = register(
            SerializeUtils.of(
                    Functions.FunctionN::serialize,
                    t -> Functions.COS.get()
            ),
            Functions.FunctionN.class,
            defaultID("cos")
    );

    public static final Factory<Dot> V_DOT = register(Dot::new, Dot.class);

    public static final Factory<Cross> V_CROSS = register(Cross::new, Cross.class);

    public static final Factory<QTransform> V_TRANSFORM = register(QTransform::new, QTransform.class);


    public static final Factory<Functions.FunctionN> TAN = register(
            SerializeUtils.of(
                    Functions.FunctionN::serialize,
                    t -> Functions.TAN.get()
            ),
            Functions.FunctionN.class,
            defaultID("tan")
    );

    public static final Factory<Functions.FunctionN> ASIN = register(Functions.ASIN, Functions.FunctionN.class, "asin");

    public static final Factory<Functions.FunctionN> ACOS = register(Functions.ACOS, Functions.FunctionN.class, "acos");

    public static final Factory<Functions.FunctionN> ATAN = register(Functions.ATAN, Functions.FunctionN.class, "atan");

    public static final Factory<FlexibleGate> F_GATE = register(
            SerializeUtils.of(
                    FlexibleGate::serialize,
                    FlexibleGate::deserialize
            ),
            FlexibleGate.class,
            defaultID("flexible_gate")
    );

    public static final Factory<Gates.Gate> AND_N = register(
            SerializeUtils.of(
                    Gates.Gate::serialize,
                    t -> Gates.AND.apply(Gates.deserializeN(t))
            ),
            Gates.Gate.class,
            defaultID("and")
    );

    public static final Factory<Gates.Gate> OR_N = register(
            SerializeUtils.of(
                    Gates.Gate::serialize,
                    t -> Gates.OR.apply(Gates.deserializeN(t))
            ),
            Gates.Gate.class,
            defaultID("or")
    );

    public static final Factory<Gates.Gate> NOT_N = register(
            SerializeUtils.of(
                    Gates.Gate::serialize,
                    t -> Gates.NOT.apply(Gates.deserializeN(t))
            ),
            Gates.Gate.class,
            defaultID("not")
    );

    public static final Factory<Gates.Gate> XOR_N = register(
            SerializeUtils.of(
                    Gates.Gate::serialize,
                    t -> Gates.XOR.apply(Gates.deserializeN(t))
            ),
            Gates.Gate.class,
            defaultID("xor")
    );

    public static final Factory<Multiplexer> MUX = register(
            SerializeUtils.of(
                    Multiplexer::serialize,
                    Multiplexer::deserialize
            ),
            Multiplexer.class,
            defaultID("mux")
    );

    public static final Factory<Shifter> SHIFTER = register(
            SerializeUtils.of(
                    Shifter::serialize,
                    Shifter::deserialize
            ),
            Shifter.class,
            defaultID("shifter")
    );

    public static final Factory<AsyncShifter> ASYNC_SHIFTER = register(
            SerializeUtils.of(
                    AsyncShifter::serialize,
                    AsyncShifter::deserialize
            ),
            AsyncShifter.class,
            defaultID("async_shifter")
    );


    private static<T extends NamedComponent> Factory<T> register(
            Serializer<T> serializer,
            Class<T> clazz,
            String ID
    ){
        if(REGISTRY.containsKey(ID)){
            throw new IllegalArgumentException("Factory with ID " + ID + " already exists.");
        }
        Factory<T> factory = new Factory<>(serializer, clazz, ID);
        REGISTRY.put(ID, factory);
        return factory;
    }

    private static <T extends NamedComponent> Factory<T> register(
            Supplier<T> argLessFactory,
            Class<T> clazz
    ){
        return register(
                SerializeUtils.of(
                        $ -> new CompoundTag(),
                        $ -> argLessFactory.get()
                ),
                clazz,
                defaultID(clazz.getSimpleName().toLowerCase())
        );
    }

    private static <T extends NamedComponent> Factory<T> register(
            Supplier<T> argLessFactory,
            Class<T> clazz,
            String ID
    ){
        return register(
                SerializeUtils.of(
                        $ -> new CompoundTag(),
                        $ -> argLessFactory.get()
                ),
                clazz,
                defaultID(ID)
        );
    }


    private static String defaultID(String name){
        return PREFIX + name;
    }

    public static<T extends NamedComponent> T restore(Summary summary, Class<T> clazz){
        Factory<?> factory = REGISTRY.get(summary.registerName());
        if (factory == null){
            throw new IllegalArgumentException("No factory registered for ID: " + summary.registerName());
        }
        NamedComponent component = factory.serializer.deserialize(summary.componentTag());
        if(!clazz.isAssignableFrom(component.getClass())){
            throw new IllegalArgumentException("Component " + component.getClass().getName() + " is not assignable to " + clazz.getName());
        }
        return clazz.cast(component);
    }


    public static<T extends NamedComponent> Serializer<T> createParamLess(Supplier<T> initializer){
        return SerializeUtils.of(
                $ -> new CompoundTag(),
                $ -> initializer.get()
        );
    }

    public static void register(){}

    public interface ComponentDeserializer{
        NamedComponent deserialize(CompoundTag tag);
    }



}
