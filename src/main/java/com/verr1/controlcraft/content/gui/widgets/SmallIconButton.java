package com.verr1.controlcraft.content.gui.widgets;

import com.simibubi.create.foundation.gui.element.ScreenElement;
import com.simibubi.create.foundation.gui.widget.IconButton;
import com.verr1.controlcraft.content.gui.layouts.api.SizedScreenElement;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;

import java.util.List;

public class SmallIconButton extends IconButton {

    protected final ScreenElement hovered;

    public SmallIconButton(
            int x, int y,
            SizedScreenElement icon
    ) {
        super(x, y, icon.width(), icon.height(), icon);
        this.hovered = icon;
    }

    public SmallIconButton(
            int x, int y,
            SizedScreenElement icon,
            SizedScreenElement hovered
    ) {
        super(x, y, icon.width(), icon.height(), icon);
        this.hovered = hovered;
    }

    public SmallIconButton(SizedScreenElement icon) {
        super(0, 0, icon.width(), icon.height(), icon);
        hovered = icon;
    }

    public SmallIconButton(SizedScreenElement icon, SizedScreenElement hovered) {
        super(0, 0, icon.width(), icon.height(), icon);
        this.hovered = hovered;
    }

    @Override
    public void doRender(GuiGraphics graphics, int mouseX, int mouseY, float partialTicks) {
        if (this.visible) {
            if(isHovered){
                this.hovered.render(graphics, this.getX(), this.getY());
            }else{
                this.icon.render(graphics, this.getX(), this.getY());
            }
        }
    }

    public SmallIconButton withToolTips(List<Component> tooltips) {
        this.getToolTip().clear();
        this.getToolTip().addAll(tooltips);
        return this;

    }

}
