package com.verr1.controlcraft.foundation.cimulink.core.records;

import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import net.minecraft.nbt.CompoundTag;

public record ComponentPortName(String componentName, String portName) {
    public static final Serializer<ComponentPortName> SER = SerializeUtils.of(
            ComponentPortName::serialize,
            ComponentPortName::deserialize
    );


    @Override
    public boolean equals(Object obj) {
        if (obj instanceof ComponentPortName other) {
            return componentName.equals(other.componentName) &&
                    portName.equals(other.portName);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return componentName.hashCode() ^ portName.hashCode();
    }

    @Override
    public String toString() {
        return "[" + componentName + " | " + portName + "]";
    }

    public CompoundTag serialize(){
        CompoundTag tag = new CompoundTag();
        tag.putString("component_name", componentName);
        tag.putString("port_name", portName);
        return tag;
    }

    public static ComponentPortName deserialize(CompoundTag tag){
        return new ComponentPortName(
                tag.getString("component_name"),
                tag.getString("port_name")
        );
    }

}
