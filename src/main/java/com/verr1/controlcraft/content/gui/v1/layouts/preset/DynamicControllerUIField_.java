package com.verr1.controlcraft.content.gui.v1.layouts.preset;

import com.simibubi.create.foundation.gui.widget.Label;
import com.verr1.controlcraft.content.gui.v1.factory.GenericUIFactory;
import com.verr1.controlcraft.content.gui.v1.layouts.NetworkUIPort;
import com.verr1.controlcraft.content.gui.v1.layouts.api.TitleLabelProvider;
import com.verr1.controlcraft.content.gui.v1.widgets.FormattedLabel;
import com.verr1.controlcraft.foundation.api.IControllerProvider;
import com.verr1.controlcraft.foundation.api.ISerializableDynamicController;
import com.verr1.controlcraft.foundation.data.control.PID;
import com.verr1.controlcraft.foundation.type.descriptive.ExposedFieldType;
import com.verr1.controlcraft.foundation.type.descriptive.UIContents;
import com.verr1.controlcraft.utils.ParseUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.gui.layouts.GridLayout;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;

import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class DynamicControllerUIField_ extends NetworkUIPort<CompoundTag> implements
        ISerializableDynamicController, TitleLabelProvider
{

    FormattedLabel title = UIContents.PID_CONTROLLER.toDescriptiveLabel();
    FormattedLabel pLabel = ExposedFieldType.P.toDescriptiveLabel();
    FormattedLabel iLabel = ExposedFieldType.I.toDescriptiveLabel();
    FormattedLabel dLabel = ExposedFieldType.D.toDescriptiveLabel();
    EditBox pField;
    EditBox iField;
    EditBox dField;

    PID pid;

    public DynamicControllerUIField_(Consumer<CompoundTag> write, Supplier<CompoundTag> read, int fieldLength) {
        super(write, read);
        Font font = Minecraft.getInstance().font;
        pField = new EditBox(font, 0, 0, fieldLength, 10, Component.literal(""));
        iField = new EditBox(font, 0, 0, fieldLength, 10, Component.literal(""));
        dField = new EditBox(font, 0, 0, fieldLength, 10, Component.literal(""));
        GenericUIFactory.alignLabel(titles());
    }

    public DynamicControllerUIField_(Supplier<IControllerProvider> supplier){
        this(
                t -> Optional.ofNullable(supplier.get()).ifPresent(it -> it.getController().deserialize(t)),
                () -> Optional.ofNullable(supplier.get()).map(it -> it.getController().serialize()).orElse(new CompoundTag()),
                30
        );
    }

    @Override
    protected void initLayout(GridLayout layoutToFill) {
        layoutToFill.addChild(title, 0, 0, 1, 5);
        layoutToFill.addChild(pLabel, 1, 0);
        layoutToFill.addChild(pField, 1, 1);
        layoutToFill.addChild(iLabel, 1, 2);
        layoutToFill.addChild(iField, 1, 3);
        layoutToFill.addChild(dLabel, 1, 4);
        layoutToFill.addChild(dField, 1, 5);
        layoutToFill.rowSpacing(4).columnSpacing(4);
    }

    @Override
    protected CompoundTag readGUI() {
        PID(new PID(
                ParseUtils.tryParseDouble(pField.getValue()),
                ParseUtils.tryParseDouble(iField.getValue()),
                ParseUtils.tryParseDouble(dField.getValue())
        ));
        return ISerializableDynamicController.super.serialize();
    }

    @Override
    protected void writeGUI(CompoundTag value) {
        ISerializableDynamicController.super.deserialize(value);
        pField.setValue("" + PID().p());
        iField.setValue("" + PID().i());
        dField.setValue("" + PID().d());
    }

    @Override
    public void PID(PID pid) {
        this.pid = pid;
    }

    @Override
    public PID PID() {
        return pid;
    }

    @Override
    public Label title() {
        return title;
    }

    @Override
    public Label[] titles() {
        return new Label[]{pLabel, iLabel, dLabel};
    }
}
