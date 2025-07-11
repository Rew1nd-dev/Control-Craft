package com.verr1.controlcraft.foundation.type.descriptive;

import com.verr1.controlcraft.content.gui.layouts.api.Descriptive;
import com.verr1.controlcraft.utils.LangUtils;
import net.minecraft.network.chat.Component;

import java.util.List;

import static com.verr1.controlcraft.utils.ComponentUtils.literals;

public enum ConstraintMode implements Descriptive<ConstraintMode> {
    CONSTRAINT(literals("Using Constraint", "High Repulse")),
    KINEMATIC(literals("Using Kinematic Target", "No Repulse, No Physics Feedback")),;

    ConstraintMode(List<Component> description) {
        LangUtils.registerDefaultName(clazz(), this, Component.literal(name()));
        LangUtils.registerDefaultDescription(clazz(), this, description);
    }

    @Override
    public ConstraintMode self() {
        return this;
    }

    @Override
    public Class<ConstraintMode> clazz() {
        return ConstraintMode.class;
    }

    public static void register(){}

}
