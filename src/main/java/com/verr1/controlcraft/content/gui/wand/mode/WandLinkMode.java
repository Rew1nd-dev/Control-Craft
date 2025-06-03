package com.verr1.controlcraft.content.gui.wand.mode;

import com.verr1.controlcraft.content.gui.wand.mode.base.WandAbstractDualSelectionMode;
import com.verr1.controlcraft.foundation.api.IWandMode;
import com.verr1.controlcraft.foundation.data.WandSelection;

public class WandLinkMode extends WandAbstractDualSelectionMode {
    public static final String ID = "wand_link_mode";

    public static WandLinkMode instance;

    public static void createInstance(){
        instance = new WandLinkMode();
    }

    @Override
    public IWandMode getInstance() {
        return instance;
    }

    @Override
    public String getID() {
        return ID;
    }

    @Override
    protected void confirm(WandSelection x, WandSelection y) {

    }
}
