package com.verr1.controlcraft.content.gui.layouts.element;

import com.verr1.controlcraft.content.blocks.motor.AbstractMotor;
import com.verr1.controlcraft.content.gui.factory.Converter;
import com.verr1.controlcraft.content.gui.layouts.element.general.TypedUIPort;
import com.verr1.controlcraft.content.gui.widgets.FormattedLabel;
import com.verr1.controlcraft.content.gui.widgets.SmallCheckbox;
import com.verr1.controlcraft.foundation.data.constraint.AngleLimit;
import com.verr1.controlcraft.foundation.type.descriptive.UIContents;
import com.verr1.controlcraft.utils.ParseUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.gui.layouts.GridLayout;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;

public class AngleLimitUIPort extends TypedUIPort<AngleLimit> {

    protected final FormattedLabel minLabel = UIContents.FROM.convertTo(Converter::titleStyle).toDescriptiveLabel();
    protected final EditBox minField = new EditBox(Minecraft.getInstance().font, 0, 0, 60, 10, Component.literal(""));

    protected final FormattedLabel maxLabel = UIContents.TO.convertTo(Converter::titleStyle).toDescriptiveLabel();
    protected final EditBox maxField = new EditBox(Minecraft.getInstance().font, 0, 0, 60, 10, Component.literal(""));

    protected final FormattedLabel clockWiseLabel = UIContents.CLOCKWISE.convertTo(Converter::titleStyle).toDescriptiveLabel();
    protected final SmallCheckbox clockWiseSelector = new SmallCheckbox();

    public AngleLimitUIPort(BlockPos boundPos) {
        super(boundPos, AbstractMotor.ANGLE_LIMIT, AngleLimit.class, AngleLimit.FREE);
    }

    @Override
    protected void initLayout(GridLayout layoutToFill) {

        layoutToFill.addChild(minLabel, 0, 0);
        layoutToFill.addChild(minField, 0, 1);
        layoutToFill.addChild(maxLabel, 1, 0);
        layoutToFill.addChild(maxField, 1, 1);
        layoutToFill.addChild(clockWiseLabel, 2, 0);
        layoutToFill.addChild(clockWiseSelector, 2, 1);

        layoutToFill.rowSpacing(4).columnSpacing(4);
    }

    @Override
    protected AngleLimit readGUI() {
        return AngleLimit.fromTo(
                ParseUtils.tryParseDouble(minField.getValue()),
                ParseUtils.tryParseDouble(maxField.getValue()),
                clockWiseSelector.selected()
        );
    }

    @Override
    protected void writeGUI(AngleLimit value) {
        minField.setValue(String.valueOf(value.low()));
        maxField.setValue(String.valueOf(value.high()));
        clockWiseSelector.setSelected(value.low() < value.high());
    }
}
