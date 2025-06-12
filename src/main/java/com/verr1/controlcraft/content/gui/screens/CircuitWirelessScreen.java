package com.verr1.controlcraft.content.gui.screens;

import com.simibubi.create.foundation.gui.menu.AbstractSimiContainerScreen;
import com.verr1.controlcraft.foundation.cimulink.game.misc.CircuitWirelessMenu;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class CircuitWirelessScreen extends AbstractSimiContainerScreen<CircuitWirelessMenu> {


    public CircuitWirelessScreen(CircuitWirelessMenu container, Inventory inv, Component title) {
        super(container, inv, title);
    }

    @Override
    protected void renderBg(GuiGraphics p_283065_, float p_97788_, int p_97789_, int p_97790_) {

    }
}
