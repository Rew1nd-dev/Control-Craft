package com.verr1.controlcraft.content.items;

import com.verr1.controlcraft.registry.ControlCraftManuals;
import li.cil.manual.api.ManualModel;
import li.cil.manual.api.prefab.Manual;
import li.cil.manual.api.prefab.item.AbstractManualItem;
import org.jetbrains.annotations.NotNull;

public class CommonManualItem extends AbstractManualItem {

    public CommonManualItem(Properties properties) {
        super(properties);
    }

    @Override
    protected @NotNull ManualModel getManualModel() {
        return ControlCraftManuals.MANUAL.get();
    }
}
