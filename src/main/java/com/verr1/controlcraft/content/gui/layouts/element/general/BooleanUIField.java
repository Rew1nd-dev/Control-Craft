package com.verr1.controlcraft.content.gui.layouts.element.general;

import com.simibubi.create.foundation.gui.widget.Label;
import com.verr1.controlcraft.content.gui.layouts.api.LabelProvider;
import com.verr1.controlcraft.content.gui.layouts.api.TitleLabelProvider;
import com.verr1.controlcraft.content.gui.widgets.FormattedLabel;
import com.verr1.controlcraft.content.gui.widgets.SmallCheckbox;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import net.minecraft.client.gui.layouts.GridLayout;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;

public class BooleanUIField extends TypedUIPort<Boolean> implements TitleLabelProvider {

    private final FormattedLabel title;
    private final SmallCheckbox field;
    private boolean inverted = false;

    public BooleanUIField(BlockPos boundPos, NetworkKey key, LabelProvider titleText) {
        super(boundPos, key, Boolean.class, false);
        title = titleText.toDescriptiveLabel();
        field = new SmallCheckbox(0, 0, 60, 10, Component.literal(""), false);
    }

    public BooleanUIField inverted(){
        inverted = true;
        return this;
    }

    @Override
    public void initLayout(GridLayout gridLayout){
        gridLayout.addChild(title, 0, 0);
        gridLayout.addChild(field, 0, 1);
        gridLayout.rowSpacing(4);

    }


    public Label getLabel(){
        return title;
    }

    public SmallCheckbox getField(){
        return field;
    }

    @Override
    public Boolean readGUI() {
        return inverted ^ field.selected();
    }

    @Override
    public void writeGUI(Boolean value) {
        field.setSelected(inverted ^ value);
    }

    @Override
    public Label title() {
        return title;
    }
}
