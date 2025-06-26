package com.verr1.controlcraft.foundation.cimulink.game.port.types;

import com.verr1.controlcraft.content.gui.layouts.api.Descriptive;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.vectors.Cross;
import com.verr1.controlcraft.foundation.cimulink.core.components.vectors.Dot;
import com.verr1.controlcraft.foundation.cimulink.core.components.vectors.QTransform;
import com.verr1.controlcraft.foundation.cimulink.game.ComponentInstances;
import com.verr1.controlcraft.utils.LangUtils;
import net.minecraft.network.chat.Component;

import java.util.List;

public enum VectorTypes implements Inspectable<NamedComponent>, Descriptive<VectorTypes> {
    DOT(
            ComponentInstances.Inspector.of(Dot::new),
            List.of(Component.literal("Output the dot product of two vectors"))
    ),

    CROSS(
            ComponentInstances.Inspector.of(Cross::new),
            List.of(Component.literal("Output the cross product of two vectors"))
    ),

    Q_TRANSFORM(
            ComponentInstances.Inspector.of(QTransform::new),
            List.of(Component.literal("Output the quaternion transformation of a vector"))
    )
    ;


    final ComponentInstances.Inspector<NamedComponent> inspector;

    VectorTypes(
            ComponentInstances.Inspector<NamedComponent> inspector,
            List<Component> description
    ) {
        this.inspector = inspector;
        LangUtils.registerDefaultName(clazz(), this, Component.literal(name().toUpperCase()));
        LangUtils.registerDefaultDescription(clazz(), this, description);
    }


    @Override
    public  VectorTypes self() {
        return this;
    }

    @Override
    public Class<VectorTypes> clazz() {
        return VectorTypes.class;
    }

    @Override
    public ComponentInstances.Inspector<NamedComponent> inspector() {
        return inspector;
    }

    public static void register() {

    }
        // This method can be used to trigger any registration logic if needed

}
