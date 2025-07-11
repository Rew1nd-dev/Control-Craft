package com.verr1.controlcraft.foundation.cimulink.core.registry;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.Summary;
import com.verr1.controlcraft.utils.Serializer;
import net.minecraft.nbt.CompoundTag;

public class Factory<T extends NamedComponent> implements CimulinkFactory.ComponentDeserializer {
    Serializer<T> serializer;
    Class<T> clazz;
    String ID;

    Factory(Serializer<T> serializer, Class<T> clazz, String ID) {
        this.ID = ID;
        this.serializer = serializer;
        this.clazz = clazz;
    }

    public String getID() {
        return ID;
    }

    public Summary summarize(NamedComponent component) {
        if (!clazz.isAssignableFrom(component.getClass())) {
            throw new IllegalArgumentException("Component " + component.getClass().getName() + " is not assignable to " + clazz.getName());
        }
        return new Summary(
                ID,
                serializer.serialize(clazz.cast(component))
        );
    }

    @Override
    public NamedComponent deserialize(CompoundTag tag) {
        return serializer.deserialize(tag);
    }
}
