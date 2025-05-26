package com.verr1.controlcraft.content.gui.layouts.element;

import com.simibubi.create.foundation.gui.widget.Label;
import com.verr1.controlcraft.content.blocks.receiver.PeripheralInterfaceBlockEntity;
import com.verr1.controlcraft.content.gui.factory.Converter;
import com.verr1.controlcraft.content.gui.layouts.api.TitleLabelProvider;
import com.verr1.controlcraft.content.gui.widgets.FormattedLabel;
import com.verr1.controlcraft.foundation.managers.PeripheralNetwork;
import com.verr1.controlcraft.foundation.type.descriptive.UIContents;
import com.verr1.controlcraft.utils.ParseUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.gui.layouts.GridLayout;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;

public class PeripheralKeyUIField extends TypedUIPort<PeripheralNetwork.PeripheralKey> implements TitleLabelProvider {

    private final FormattedLabel protocolLabel = UIContents.PROTOCOL.toDescriptiveLabel().withTextStyle(Converter::titleStyle);
    private final FormattedLabel nameLabel = UIContents.NAME.toDescriptiveLabel().withTextStyle(Converter::titleStyle);
    private final EditBox protocolField = new EditBox(Minecraft.getInstance().font, 0, 0, 60, 10, Component.literal(""));

    public EditBox getNameField() {
        return nameField;
    }

    public EditBox getProtocolField() {
        return protocolField;
    }

    public FormattedLabel getNameLabel() {
        return nameLabel;
    }

    public FormattedLabel getProtocolLabel() {
        return protocolLabel;
    }

    private final EditBox nameField = new EditBox(Minecraft.getInstance().font, 0, 0, 60, 10, Component.literal(""));

    public PeripheralKeyUIField(BlockPos boundPos) {
        super(
                boundPos,
                PeripheralInterfaceBlockEntity.PERIPHERAL,
                PeripheralNetwork.PeripheralKey.class,
                PeripheralNetwork.PeripheralKey.NULL
        );
    }

    @Override
    protected void initLayout(GridLayout layoutToFill) {
        layoutToFill.addChild(protocolLabel, 0, 0);
        layoutToFill.addChild(protocolField, 0, 1);
        layoutToFill.addChild(nameLabel, 1, 0);
        layoutToFill.addChild(nameField, 1, 1);
        layoutToFill.columnSpacing(4);
        layoutToFill.rowSpacing(2);
    }

    @Override
    protected PeripheralNetwork.PeripheralKey readGUI() {
        String name = nameField.getValue();
        long protocol = ParseUtils.tryParseLong(protocolField.getValue());
        return new PeripheralNetwork.PeripheralKey(protocol, name);
    }

    @Override
    protected void writeGUI(PeripheralNetwork.PeripheralKey value) {
        nameField.setValue(value.name());
        protocolField.setValue("" + value.protocol());
    }

    @Override
    public Label title() {
        return protocolLabel;
    }

    @Override
    public Label[] titles() {
        return new Label[]{protocolLabel, nameLabel};
    }

}
