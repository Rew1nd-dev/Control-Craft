package com.verr1.controlcraft.foundation.cimulink.game.port.types;

import com.verr1.controlcraft.content.gui.layouts.api.Descriptive;
import com.verr1.controlcraft.utils.LangUtils;
import net.minecraft.network.chat.Component;

import java.util.List;

import static com.verr1.controlcraft.utils.ComponentUtils.literals;

public enum AnalogGroups implements Descriptive<AnalogGroups>, EnumGroup<AnalogTypes> {


    Basic(
            literals("Basic analog operations")
    ),
    Trigonometric(
            literals("Trigonometric functions")
    ),
    Vector(
            literals("Vector operations")
    );




    AnalogGroups(List<Component> description) {
        LangUtils.registerDefaultName(clazz(), this, Component.literal(name().toUpperCase()));
        LangUtils.registerDefaultDescription(clazz(), this, description);
    }

    @Override
    public AnalogGroups self() {
        return this;
    }

    @Override
    public Class<AnalogGroups> clazz() {
        return AnalogGroups.class;
    }



    @Override
    public AnalogTypes[] members() {
        return switch (this){
            case Basic -> AnalogTypes.BASIC;
            case Trigonometric -> AnalogTypes.TRIGONOMETRIC;
            case Vector -> AnalogTypes.VECTOR;
        };
    }

    public static void register() {

    }

}
