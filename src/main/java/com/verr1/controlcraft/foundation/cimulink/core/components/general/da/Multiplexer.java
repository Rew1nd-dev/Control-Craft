package com.verr1.controlcraft.foundation.cimulink.core.components.general.da;


import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.digital.Decoder;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.Combinational;
import com.verr1.controlcraft.foundation.cimulink.core.records.ComponentPortName;
import com.verr1.controlcraft.foundation.cimulink.core.registry.CimulinkFactory;
import com.verr1.controlcraft.foundation.cimulink.core.registry.Factory;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.nbt.CompoundTag;

import java.util.List;

public class Multiplexer extends Combinational {

    private final int bits;


    public Multiplexer(int n) {
        super(
                ArrayUtils.flatten(
                        ArrayUtils.createWithPrefix(
                                "sel_",
                                n
                        ),
                        ArrayUtils.createInputNames(1 << n)
                ),
                ArrayUtils.SINGLE_OUTPUT
        );
        bits = n;
    }

    public String dat(int i){
        return in(i + bits);
    }

    public String sel(int i){
        return in(i);
    }

    public ComponentPortName __sel(int i){
        return __in(i);
    }

    public ComponentPortName __dat(int i){
        return __in(i + bits);
    }

    @Override
    protected List<Double> transform(List<Double> inputs) {
        List<Boolean> sel = ArrayUtils.mapToList(
                inputs.subList(0, bits),
                d -> d > 0.5
        );

        List<Double> dat = inputs.subList(bits, inputs.size());

        return List.of(transformInternal(sel, dat));
    }

    public CompoundTag serialize(){
        return CompoundTagBuilder.create()
                .withCompound("bits", SerializeUtils.INT.serialize(bits))
                .build();
    }

    public static Multiplexer deserialize(CompoundTag tag){
        return new Multiplexer(
                SerializeUtils.INT.deserializeOrElse(tag.getCompound("bits"), 1)
        );
    }

    protected Double transformInternal(List<Boolean> sel, List<Double> dat) {
        return dat.get(Decoder.decode(sel));

    }

    @Override
    public Factory<? extends NamedComponent> factory() {
        return CimulinkFactory.MUX;
    }
}
