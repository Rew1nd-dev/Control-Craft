package com.verr1.controlcraft.content.gui.wand.mode;

import com.verr1.controlcraft.content.gui.wand.mode.base.WandAbstractDualSelectionMode;
import com.verr1.controlcraft.foundation.api.IWandMode;
import com.verr1.controlcraft.foundation.data.WandSelection;

public class WandCompileMode extends WandAbstractDualSelectionMode {
    public static final String ID = "circuit_compile";

    public static WandCompileMode instance;

    public static void createInstance(){
        instance = new WandCompileMode();
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
