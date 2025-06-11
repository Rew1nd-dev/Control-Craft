package com.verr1.controlcraft.foundation.cimulink.game;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.analog.Functions;
import com.verr1.controlcraft.foundation.cimulink.core.components.digital.BooleanCombinational;
import com.verr1.controlcraft.foundation.cimulink.core.components.digital.BooleanTemporal;
import com.verr1.controlcraft.foundation.cimulink.core.components.digital.ff.*;
import com.verr1.controlcraft.foundation.cimulink.core.components.digital.gates.Gates;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.Temporal;
import com.verr1.controlcraft.foundation.cimulink.core.components.sources.DirectCurrent;

import java.util.List;
import java.util.function.Supplier;

public class ComponentInstances {

    public static final Inspector<BooleanCombinational> AND2 = Inspector.of(() -> Gates.AND.apply(2));
    public static final Inspector<BooleanCombinational> OR2 = Inspector.of(() -> Gates.OR.apply(2));
    public static final Inspector<BooleanCombinational> XOR2 = Inspector.of(() -> Gates.XOR.apply(2));
    public static final Inspector<BooleanCombinational> NOT = Inspector.of(() -> Gates.NOT.apply(1));

    public static final Inspector<NamedComponent> D_FF = Inspector.ofDirect(FlipFlops.D_FF);
    public static final Inspector<NamedComponent> T_FF = Inspector.ofDirect(FlipFlops.T_FF);
    public static final Inspector<NamedComponent> JK_FF = Inspector.ofDirect(FlipFlops.JK_FF);
    public static final Inspector<NamedComponent> RS_FF = Inspector.ofDirect(FlipFlops.RS_FF);

    public static final Inspector<NamedComponent> ASYNC_D_FF = Inspector.of(AsyncDFlipFlop::new);
    public static final Inspector<NamedComponent> ASYNC_T_FF = Inspector.of(AsyncTFlipFlop::new);
    public static final Inspector<NamedComponent> ASYNC_JK_FF = Inspector.of(AsyncJKFlipFlop::new);
    public static final Inspector<NamedComponent> ASYNC_RS_FF = Inspector.of(AsyncRSFlipFlop::new);


    public static final Inspector<NamedComponent> PRODUCT = Inspector.of(() -> Functions.PRODUCT.apply(2));
    public static final Inspector<NamedComponent> MAX = Inspector.of(() -> Functions.MAX.apply(2));
    public static final Inspector<NamedComponent> MIN = Inspector.of(() -> Functions.MIN.apply(2));
    public static final Inspector<NamedComponent> ANGLE_FIX = Inspector.of(Functions.ANGLE_FIX::get);



    public static class Inspector<T extends NamedComponent>{

        private final NamedComponent instance;
        private final Supplier<T> factory;

        Inspector(Supplier<T> factory){
            this.instance = factory.get();
            this.factory = factory;
        }

        public T get(){
            return factory.get();
        }

        public List<String> inputs(){
            return instance.inputs();
        }

        public List<String> outputs(){
            return instance.outputs();
        }

        public static<S extends NamedComponent> Inspector<S> of(Supplier<S> factory){
            return new Inspector<>(factory);
        }

        public static<S extends NamedComponent> Inspector<NamedComponent> ofDirect(Supplier<S> factory){
            return new Inspector<>(factory::get);
        }
    }


}
