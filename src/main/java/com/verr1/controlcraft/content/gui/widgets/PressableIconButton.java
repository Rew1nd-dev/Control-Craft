package com.verr1.controlcraft.content.gui.widgets;

import com.mojang.blaze3d.systems.RenderSystem;
import com.simibubi.create.foundation.gui.AllGuiTextures;
import com.simibubi.create.foundation.gui.element.ScreenElement;
import com.simibubi.create.foundation.gui.widget.IconButton;
import com.verr1.controlcraft.utils.LatchBoolean;
import net.minecraft.client.gui.GuiGraphics;

public class PressableIconButton extends IconButton {

    protected final ScreenElement pressed;
    protected final ScreenElement released;
    protected final LatchBoolean pressedLatch = new LatchBoolean(5);

    public PressableIconButton(int x, int y, ScreenElement released, ScreenElement pressed) {
        super(x, y, released);
        this.released = released;
        this.pressed = pressed;
    }

    @Override
    public void tick() {
        super.tick();
        pressedLatch.tick();
    }

    @Override
    public void onClick(double mouseX, double mouseY) {
        super.onClick(mouseX, mouseY);
        pressedLatch.latch();
    }

    @Override
    public void doRender(GuiGraphics graphics, int mouseX, int mouseY, float partialTicks) {
        if (visible) {
            isHovered = mouseX >= getX() && mouseY >= getY() && mouseX < getX() + width && mouseY < getY() + height;

//            AllGuiTextures button = !active ? AllGuiTextures.BUTTON_DOWN
//                    : isMouseOver(mouseX, mouseY) ? AllGuiTextures.BUTTON_HOVER : AllGuiTextures.BUTTON;
//
//            drawBg(graphics, button);

            if(pressedLatch.peek() || isHovered){
                pressed.render(graphics, getX() + 1, getY() + 1);
            }else{
                released.render(graphics, getX() + 1, getY() + 1);
            }


        }
    }
}
