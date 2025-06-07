package com.verr1.controlcraft.foundation.cimulink.game.port.types;

import com.verr1.controlcraft.content.gui.layouts.api.Descriptive;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.ComponentInstances;
import com.verr1.controlcraft.utils.LangUtils;
import net.minecraft.network.chat.Component;

import java.util.List;

public enum AnalogTypes implements Inspectable<NamedComponent>, Descriptive<AnalogTypes> {
    ;


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
}
