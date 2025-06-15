package com.verr1.controlcraft.content.gui.layouts.element;

import com.simibubi.create.foundation.gui.widget.Label;
import com.verr1.controlcraft.content.blocks.receiver.PeripheralInterfaceBlockEntity;
import com.verr1.controlcraft.content.gui.factory.Converter;
import com.verr1.controlcraft.content.gui.layouts.api.TitleLabelProvider;
import com.verr1.controlcraft.content.gui.layouts.element.general.TypedUIPort;
import com.verr1.controlcraft.content.gui.widgets.FormattedLabel;
import com.verr1.controlcraft.foundation.managers.PeripheralNetwork;
import com.verr1.controlcraft.foundation.type.descriptive.UIContents;
import net.minecraft.client.gui.layouts.GridLayout;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;

public class PeripheralKeyUIView extends TypedUIPort<PeripheralNetwork.PeripheralKey> implements TitleLabelProvider {

    private final FormattedLabel protocolLabel = UIContents.PROTOCOL.toDescriptiveLabel().withTextStyle(Converter::titleStyle);
    private final FormattedLabel nameLabel = UIContents.NAME.toDescriptiveLabel().withTextStyle(Converter::titleStyle);
    private final FormattedLabel protocolField = new FormattedLabel(60, 10, Component.literal(""));
    private final FormattedLabel nameField = new FormattedLabel(60, 10, Component.literal(""));

    public FormattedLabel getNameField() {
        return nameField;
    }

    public FormattedLabel getProtocolField() {
        return protocolField;
    }

    public FormattedLabel getNameLabel() {
        return nameLabel;
    }

    public FormattedLabel getProtocolLabel() {
        return protocolLabel;
    }



    public PeripheralKeyUIView(BlockPos boundPos) {
        super(
                boundPos,
                PeripheralInterfaceBlockEntity.VALID_PERIPHERAL,
                PeripheralNetwork.PeripheralKey.class,
                PeripheralNetwork.PeripheralKey.NULL
        );
    }

    @Override
    public void onScreenTick() {
        readToLayout();
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
        return null;
    }

    @Override
    protected void writeGUI(PeripheralNetwork.PeripheralKey value) {
        nameField.setText(Component.literal(value.name()));
        protocolField.setText(Component.literal("" + value.protocol()));
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
