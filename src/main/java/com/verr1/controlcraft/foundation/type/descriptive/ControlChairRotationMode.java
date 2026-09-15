package com.verr1.controlcraft.foundation.type.descriptive;

import com.verr1.controlcraft.content.gui.layouts.api.Descriptive;
import com.verr1.controlcraft.utils.LangUtils;
import net.minecraft.network.chat.Component;

import java.util.List;

import static com.verr1.controlcraft.utils.ComponentUtils.literals;

public enum ControlChairRotationMode implements Descriptive<ControlChairRotationMode> {
    FOLLOW_SHIP(
            Component.literal("Follow Ship"),
            literals("Rotate View Along With The Mounted Ship")
    ),
    ABSOLUTE(
            Component.literal("Absolute"),
            literals("Keep View Rotation Independent From The Mounted Ship")
    );

    ControlChairRotationMode(Component name, List<Component> description) {
        LangUtils.registerDefaultName(clazz(), self(), name);
        LangUtils.registerDefaultDescription(clazz(), self(), description);
    }

    @Override
    public ControlChairRotationMode self() {
        return this;
    }

    @Override
    public Class<ControlChairRotationMode> clazz() {
        return ControlChairRotationMode.class;
    }

    public static void register() {
        LangUtils.registerDefaultDescription(ControlChairRotationMode.class, literals("Control Chair Rotation Settings"));
    }
}
