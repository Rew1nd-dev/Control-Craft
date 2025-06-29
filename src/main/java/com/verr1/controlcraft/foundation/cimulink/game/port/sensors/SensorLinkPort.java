package com.verr1.controlcraft.foundation.cimulink.game.port.sensors;

import com.verr1.controlcraft.content.links.sensor.SensorBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.port.SwitchableLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.SensorTypes;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.nbt.CompoundTag;

public class SensorLinkPort extends SwitchableLinkPort<SensorTypes> {

    public SensorLinkPort(SensorBlockEntity sbe) {
        super(SensorTypes.OMEGA, t -> create(t, sbe));
        // this.sensorBlockEntity = sbe;
    }

    public static NamedComponent create(SensorTypes t, SensorBlockEntity sbe){
        return switch (t){
            case OMEGA -> new OmegaSensor(sbe);
            case VELOCITY -> new VelocitySensor(sbe);
            case ROTATION -> new RotationSensor(sbe);
            case EULER_YXZ -> new EulerSensor(sbe);
        };
    }

    public void setLocal(boolean local){
        if(__raw() instanceof OmegaSensor os)os.setLocal(local);
        else if(__raw() instanceof VelocitySensor vs)vs.setLocal(local);
    }

    public boolean local(){
        if(__raw() instanceof OmegaSensor os)return os.local();
        else if(__raw() instanceof VelocitySensor vs)return vs.local();
        return false;
    }

    @Override
    protected Class<SensorTypes> clazz() {
        return SensorTypes.class;
    }

    @Override
    public CompoundTag serialize() {
        return CompoundTagBuilder.create()
                .withCompound("super", super.serialize())
                .withCompound("transform", SerializeUtils.BOOLEAN.serialize(local()))
                .build();
    }

    @Override
    public void deserialize(CompoundTag tag) {
        super.deserialize(tag.getCompound("super"));
        setLocal(SerializeUtils.BOOLEAN.deserialize(tag.getCompound("transform")));
    }

}
