package com.verr1.controlcraft.foundation.cimulink.game.port.types;

import com.verr1.controlcraft.content.gui.layouts.api.Descriptive;
import com.verr1.controlcraft.utils.LangUtils;
import net.minecraft.network.chat.Component;

import java.util.List;

import static com.verr1.controlcraft.utils.ComponentUtils.literals;

public enum SensorTypes implements Descriptive<SensorTypes> {
    OMEGA(literals("Angular Velocity Sensor")),
    VELOCITY(literals("Velocity Sensor")),
    ROTATION(literals("Rotation Sensor")),
    EULER_YXZ(literals("Euler Angle Sensor, Y:yaw -> X:pitch -> Z:roll")),
    ;


    SensorTypes(List<Component> description) {
        LangUtils.registerDefaultName(clazz(), this, Component.literal(name().toUpperCase()));
        LangUtils.registerDefaultDescription(clazz(), this, description);
    }

    @Override
    public SensorTypes self() {
        return this;
    }

    @Override
    public Class<SensorTypes> clazz() {
        return SensorTypes.class;
    }

    public static void register(){}
}
