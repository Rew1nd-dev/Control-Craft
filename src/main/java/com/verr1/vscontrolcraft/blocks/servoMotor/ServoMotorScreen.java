package com.verr1.vscontrolcraft.blocks.servoMotor;

import com.simibubi.create.foundation.gui.AllGuiTextures;
import com.simibubi.create.foundation.gui.AllIcons;
import com.simibubi.create.foundation.gui.widget.IconButton;
import com.simibubi.create.foundation.gui.widget.Label;
import com.simibubi.create.foundation.utility.Components;
import com.verr1.vscontrolcraft.base.Servo.PIDControllerScreen;
import com.verr1.vscontrolcraft.base.Servo.PIDControllerType;
import com.verr1.vscontrolcraft.base.UltraTerminal.ExposedFieldType;
import com.verr1.vscontrolcraft.network.packets.BlockBoundPacketType;
import com.verr1.vscontrolcraft.network.packets.BlockBoundServerPacket;
import com.verr1.vscontrolcraft.registry.AllBlocks;
import com.verr1.vscontrolcraft.registry.AllGuiLabels;
import com.verr1.vscontrolcraft.registry.AllPackets;
import com.verr1.vscontrolcraft.utils.Util;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.ItemStack;

public class ServoMotorScreen extends PIDControllerScreen {

    private final double offset;
    private final boolean isAdjustingAngle;
    private final boolean isCheatMode;

    protected EditBox oField;

    protected Label oLabel;
    protected Label mLabel;
    protected Label cLabel;
    protected Label cField;

    protected IconButton toggleCheatMode;

    public ServoMotorScreen(
            BlockPos entityPos,
            double p,
            double i,
            double d,
            double v,
            double t,
            double offset,
            boolean isAdjustingAngle,
            boolean isCheatMode

    ) {
        super(entityPos, p, i, d, v, t);
        this.offset = offset;
        this.isAdjustingAngle = isAdjustingAngle;
        this.isCheatMode =isCheatMode;
        // cycleMode.visible = true;
    }

    @Override
    public void initWidgets() {
        super.initWidgets();
        oField = new EditBox(font, 0, 0, common_input_width, common_input_height, Components.literal("offset"));
        oField.setTextColor(-1);
        oField.setTextColorUneditable(-1);
        oField.setBordered(true);

        Component mode = isAdjustingAngle ? ExposedFieldType.MODE_ANGULAR.getComponent() : ExposedFieldType.MODE_SPEED.getComponent();
        mLabel = new Label(0, 0, mode).colored(common_label_color);
        mLabel.text = mode;
        addRenderableWidget(mLabel);

        cLabel = new Label(0, 0, ExposedFieldType.MODE_CHEAT.getComponent());
        cLabel.text = ExposedFieldType.MODE_CHEAT.getComponent();
        addRenderableWidget(cLabel);


        Component onOff= isCheatMode ? AllGuiLabels.onLabel : AllGuiLabels.offLabel;
        cField = new Label(0, 0, onOff);
        cField.text = onOff;
        addRenderableWidget(cField);

        toggleCheatMode = new IconButton(0, 0, AllIcons.I_ARM_FORCED_ROUND_ROBIN);
        toggleCheatMode.withCallback(this::toggleCheatMode);
        addRenderableWidget(toggleCheatMode);


        oField.setMaxLength(35);
        oField.setValue(String.format("%.2f", offset));
        oField.setFilter(Util::tryParseDoubleFilter);

        addRenderableWidget(oField);

        oLabel = new Label(0, 0, ExposedFieldType.OFFSET.getComponent()).colored(common_label_color);
        oLabel.text = ExposedFieldType.OFFSET.getComponent();
        addRenderableWidget(oLabel);



    }

    @Override
    public void startWindow() {
        super.startWindow();
        controlValueLayout.addChild(oField, 4, 1);
        controlValueLayout.addChild(oLabel, 4, 0);
        statisticsLayout.addChild(mLabel, 1, 0);
        statisticsLayout.addChild(cLabel, 2, 0);
        statisticsLayout.addChild(cField, 2, 1);
        controlValueLayout.rowSpacing(3);
        buttonLayout.addChild(toggleCheatMode, 0, 3);
        totalLayout.arrangeElements();
    }

    public void toggleCheatMode(){
        var p = new BlockBoundServerPacket.builder(pos, BlockBoundPacketType.TOGGLE)
                .build();
        AllPackets.getChannel().sendToServer(p);
        super.register();
        onClose();
    }

    @Override
    public void register() {
        var p = new BlockBoundServerPacket.builder(pos, BlockBoundPacketType.SETTING)
                .withDouble(Util.tryParseDouble(oField.getValue()))
                .build();
        AllPackets.getChannel().sendToServer(p);
        super.register();


    }

    @Override
    protected ItemStack renderedItem() {
        return AllBlocks.SERVO_MOTOR_BLOCK.asStack();
    }
}
