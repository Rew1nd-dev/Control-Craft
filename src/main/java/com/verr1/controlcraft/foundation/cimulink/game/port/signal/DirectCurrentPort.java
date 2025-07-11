package com.verr1.controlcraft.foundation.cimulink.game.port.signal;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.sources.DirectCurrent;
import com.verr1.controlcraft.foundation.cimulink.core.registry.Factory;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.ICompilable;
import com.verr1.controlcraft.foundation.cimulink.core.registry.CimulinkFactory;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.nbt.CompoundTag;

public class DirectCurrentPort extends BlockLinkPort implements ICompilable<DirectCurrent> {


    public DirectCurrentPort() {
        super(new DirectCurrent(0.0));
    }

    public void setValue(double value) {
        ((DirectCurrent)__raw()).setDc(value);
    }

    public double getValue() {
        return ((DirectCurrent)__raw()).dc();
    }

    @Override
    public NamedComponent create() {
        return new DirectCurrent(0.0);
    }

    @Override
    public CompoundTag serialize() {
        return CompoundTagBuilder.create()
                .withCompound("blp", super.serialize())
                .withCompound("dc", SerializeUtils.DOUBLE.serialize(getValue()))
                .build();
    }

    @Override
    public void deserialize(CompoundTag tag) {
        setValue(SerializeUtils.DOUBLE.deserialize(tag.getCompound("dc")));
        super.deserialize(tag.getCompound("blp"));
    }

    @Override
    public DirectCurrent component() {
        return (DirectCurrent)__raw();
    }

    @Override
    public Factory<DirectCurrent> factory() {
        return CimulinkFactory.DC;
    }
}
