package com.verr1.controlcraft;

import com.verr1.controlcraft.content.gui.wand.WandGUI;
import com.verr1.controlcraft.foundation.executor.Executor;
import com.verr1.controlcraft.foundation.managers.render.BezierOutliner;
import com.verr1.controlcraft.foundation.managers.render.DynamicOutliner;

public class ControlCraftClient {

    public static final WandGUI CLIENT_WAND_HANDLER = new WandGUI();
    public static final DynamicOutliner CLIENT_LERPED_OUTLINER = new DynamicOutliner();
    public static final BezierOutliner CLIENT_CURVE_OUTLINER = new BezierOutliner();

    public static final Executor CLIENT_EXECUTOR = new Executor();

    // public static final WandGUI ClientWandHandler = new WandGUI();

    public static void clientInit(){

    }


}
