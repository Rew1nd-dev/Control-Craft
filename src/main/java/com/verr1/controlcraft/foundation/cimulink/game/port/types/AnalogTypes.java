package com.verr1.controlcraft.foundation.cimulink.game.port.types;

import com.verr1.controlcraft.content.gui.layouts.api.Descriptive;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.ComponentInstances;
import com.verr1.controlcraft.utils.LangUtils;
import net.minecraft.network.chat.Component;

import java.util.List;

import static com.verr1.controlcraft.utils.ComponentUtils.literals;

public enum AnalogTypes implements Inspectable<NamedComponent>, Descriptive<AnalogTypes> {
    MIN(ComponentInstances.MIN, literals("Output the minimum value of 2 inputs")),
    MAX(ComponentInstances.MAX, literals("Output the maximum value of 2 inputs")),
    PRODUCT(ComponentInstances.PRODUCT, literals("Output the product of 2 inputs")),
    ANGLE_FIX(ComponentInstances.ANGLE_FIX, literals("Coerce the input into (-pi, pi)"));


    private final ComponentInstances.Inspector<NamedComponent> inspector;

    AnalogTypes(
            ComponentInstances.Inspector<NamedComponent> inspector,
            List<Component> description
    ) {
        this.inspector = inspector;
        LangUtils.registerDefaultName(clazz(), this, Component.literal(name().toUpperCase()));
        LangUtils.registerDefaultDescription(clazz(), this, description);
    }

    @Override
    public AnalogTypes self() {
        return this;
    }

    @Override
    public Class<AnalogTypes> clazz() {
        return AnalogTypes.class;
    }

    @Override
    public ComponentInstances.Inspector<NamedComponent> inspector() {
        return inspector;
    }

    public static void register(){}

}
