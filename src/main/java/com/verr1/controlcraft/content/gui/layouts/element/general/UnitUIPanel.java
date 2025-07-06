package com.verr1.controlcraft.content.gui.layouts.element.general;

import com.simibubi.create.foundation.gui.widget.Label;
import com.verr1.controlcraft.content.gui.layouts.api.LabelProvider;
import com.verr1.controlcraft.content.gui.layouts.api.TitleLabelProvider;
import com.verr1.controlcraft.content.gui.widgets.FormattedLabel;
import com.verr1.controlcraft.content.gui.widgets.SmallIconButton;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.registry.ControlCraftGuiTextures;
import net.minecraft.client.gui.layouts.GridLayout;
import net.minecraft.core.BlockPos;

public class UnitUIPanel extends TypedUIPanel<Double> implements TitleLabelProvider {

    protected final FormattedLabel title;
    protected final SmallIconButton unitButton = new SmallIconButton(0, 0, ControlCraftGuiTextures.SMALL_BUTTON_YES).withCallback(this::trigger);



    protected final NetworkKey key;

    public UnitUIPanel(
            BlockPos boundPos,
            NetworkKey key,
            Double defaultValue,
            LabelProvider titleProv
    ) {
        super(
                boundPos,
                key,
                Double.class,
                defaultValue
        );
        title = titleProv.toDescriptiveLabel();
        this.key = key;
    }

    public UnitUIPanel(
            BlockPos boundPos,
            NetworkKey key,
            LabelProvider titleProv
    ) {
        this(
                boundPos,
                key,
                0.0,
                titleProv
        );
    }

    public NetworkKey key() {
        return key;
    }

    @Override
    protected void initLayout(GridLayout layoutToFill) {
        layoutToFill.addChild(title, 0, 0);
        layoutToFill.addChild(unitButton, 0, 1);
    }

    @Override
    protected Double readGUI() {
        return 0.0;
    }


    @Override
    public Label title() {
        return title;
    }
}
