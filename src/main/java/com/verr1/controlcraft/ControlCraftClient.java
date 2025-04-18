package com.verr1.controlcraft;

import com.simibubi.create.foundation.render.SuperByteBufferCache;
import com.verr1.controlcraft.content.gui.wand.WandGUI;
import com.verr1.controlcraft.foundation.executor.Executor;
import com.verr1.controlcraft.registry.ControlCraftPartialModels;
import com.verr1.controlcraft.render.CachedBufferer;

public class ControlCraftClient {
    public static final SuperByteBufferCache BUFFER_CACHE = new SuperByteBufferCache();

    public static final WandGUI CLIENT_WAND_HANDLER = new WandGUI();

    // public static final DeferralExecutor CLIENT_DEFERRAL_EXECUTOR = new DeferralExecutor();
    // public static final IntervalExecutor CLIENT_INTERVAL_EXECUTOR = new IntervalExecutor();
    public static final Executor CLIENT_EXECUTOR = new Executor();

    // public static final WandGUI ClientWandHandler = new WandGUI();

    public static void clientInit(){
        ControlCraft.LOGGER.info("Try CC Generic Block");
        BUFFER_CACHE.registerCompartment(CachedBufferer.CC_GENERIC_BLOCK);
        ControlCraft.LOGGER.info("Try CC Partial");
        BUFFER_CACHE.registerCompartment(CachedBufferer.CC_PARTIAL);
        ControlCraft.LOGGER.info("Try CC Directional Partial");
        BUFFER_CACHE.registerCompartment(CachedBufferer.CC_DIRECTIONAL_PARTIAL);
        ControlCraftPartialModels.init();

    }


}
