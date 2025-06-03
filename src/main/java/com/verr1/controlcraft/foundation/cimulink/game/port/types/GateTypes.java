package com.verr1.controlcraft.foundation.cimulink.game.port.types;

import com.verr1.controlcraft.content.gui.layouts.api.Descriptive;
import com.verr1.controlcraft.foundation.cimulink.core.components.digital.BooleanCombinational;
import com.verr1.controlcraft.foundation.cimulink.game.ComponentInstances;
import com.verr1.controlcraft.foundation.type.descriptive.GroupPolicy;
import com.verr1.controlcraft.utils.LangUtils;
import net.minecraft.network.chat.Component;

import java.util.List;

import static com.verr1.controlcraft.utils.ComponentUtils.literals;

public enum GateTypes implements Inspectable<BooleanCombinational>, Descriptive<GateTypes> {
    AND(ComponentInstances.AND2, literals("2->1 AND Gate")),
    OR(ComponentInstances.OR2, literals("2->1 OR Gate")),
    XOR(ComponentInstances.XOR2, literals("2->1 XOR Gate")),
    NOT(ComponentInstances.NOT, literals("1->1 Negate")),;

    private final ComponentInstances.Inspector<BooleanCombinational> inspector;


    GateTypes(
            ComponentInstances.Inspector<BooleanCombinational> inspector,
            List<Component> description
    ) {
        this.inspector = inspector;
        LangUtils.registerDefaultName(GateTypes.class, this, Component.literal(name().toUpperCase()));
        LangUtils.registerDefaultDescription(GateTypes.class, this, description);

    }

    @Override
    public ComponentInstances.Inspector<BooleanCombinational> inspector() {
        return inspector;
    }

    @Override
    public GateTypes self() {
        return this;
    }

    @Override
    public Class<GateTypes> clazz() {
        return GateTypes.class;
    }

    public static void register(){}
}
