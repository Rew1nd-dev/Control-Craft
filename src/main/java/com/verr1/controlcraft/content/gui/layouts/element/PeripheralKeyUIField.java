package com.verr1.controlcraft.content.gui.layouts.element;

import com.simibubi.create.foundation.gui.widget.Label;
import com.verr1.controlcraft.content.blocks.receiver.PeripheralInterfaceBlockEntity;
import com.verr1.controlcraft.content.gui.layouts.api.TitleLabelProvider;
import com.verr1.controlcraft.content.gui.widgets.FormattedLabel;
import com.verr1.controlcraft.foundation.data.PeripheralKey;
import com.verr1.controlcraft.foundation.type.descriptive.UIContents;
import com.verr1.controlcraft.utils.ParseUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.gui.layouts.GridLayout;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;

public class PeripheralKeyUIField extends TypedUIPort<PeripheralKey> implements TitleLabelProvider {

    private final FormattedLabel protocolLabel = UIContents.PROTOCOL.toDescriptiveLabel();
    private final FormattedLabel nameLabel = UIContents.NAME.toDescriptiveLabel();
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
                PeripheralKey.class,
                PeripheralKey.NULL
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
    protected PeripheralKey readGUI() {
        String name = nameField.getValue();
        long protocol = ParseUtils.tryParseLong(protocolField.getValue());
        return new PeripheralKey(name, protocol);
    }

    @Override
    protected void writeGUI(PeripheralKey value) {
        nameField.setValue(value.Name());
        protocolField.setValue("" + value.Protocol());
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
