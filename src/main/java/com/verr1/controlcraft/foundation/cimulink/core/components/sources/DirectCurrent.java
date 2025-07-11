package com.verr1.controlcraft.foundation.cimulink.core.components.sources;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.registry.CimulinkFactory;
import com.verr1.controlcraft.foundation.cimulink.core.registry.Factory;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.nbt.CompoundTag;

public class DirectCurrent extends SignalGenerator<Double> {

    private double dc = 0;

    public DirectCurrent(double dc) {
        super(() -> dc);
        this.dc = dc;
    }

    public void setDc(double dc){
        this.dc = dc;
    }

    @Override
    protected double generate(Double dc) {
        return this.dc;
    }

    @Override
    protected Double next(Double $) {
        return this.dc;
    }

    public double dc() {
        return dc;
    }

    public CompoundTag serialize(){
        return CompoundTagBuilder.create()
                .withCompound("dc", SerializeUtils.DOUBLE.serialize(dc))
                .build();
    }

    public static DirectCurrent deserialize(CompoundTag tag) {
        return new DirectCurrent(SerializeUtils.DOUBLE.deserialize(tag.getCompound("dc")));
    }

    @Override
    public Factory<? extends NamedComponent> factory() {
        return CimulinkFactory.DC;
    }
}
