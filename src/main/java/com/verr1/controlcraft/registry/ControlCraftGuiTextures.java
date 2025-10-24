package com.verr1.controlcraft.registry;

import com.mojang.blaze3d.systems.RenderSystem;
import com.simibubi.create.foundation.gui.UIRenderHelper;
import com.simibubi.create.foundation.utility.Color;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.gui.layouts.api.SizedScreenElement;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.resources.ResourceLocation;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

public enum ControlCraftGuiTextures implements SizedScreenElement {
    // SIMPLE_BACKGROUND("simple_background", 179, 133),
    // SIMPLE_BACKGROUND_HALF("simple_background_half", 87, 108),
    SIMPLE_BACKGROUND_QUARTER("simple_background_5_6", 166, 133),
    SIMPLE_BACKGROUND_LARGE("simple_background_large", 256, 133),
    SIMPLE_BACKGROUND_ONE_LINE("simple_background_one_line", 160, 29),


    SMALL_BUTTON_GREEN("icons10x10", 2, 50, 9, 13),
    SMALL_BUTTON_SELECTION("icons10x10", 12, 50, 9, 13),
    SMALL_BUTTON_NO("icons10x10", 22, 50, 9, 13),
    SMALL_BUTTON_YES("icons10x10", 32, 50, 9, 13),

    SMALL_BUTTON_RED("icons10x10", 2, 64, 9, 13),
    SMALL_BUTTON_GREEN_PRESSED("icons10x10", 2, 64, 9, 13),
    SMALL_BUTTON_SELECTION_PRESSED("icons10x10", 12, 64, 9, 13),
    SMALL_BUTTON_NO_PRESSED("icons10x10", 22, 64, 9, 13),
    SMALL_BUTTON_YES_PRESSED("icons10x10", 32, 64, 9, 13),

    // TAB_BUTTON_BACKGROUND("tab_button_background", 0, 0, 40, 12),
    // TAB_BUTTON_FRAME("tab_button_frame", 0, 0, 40, 1),
    TAB_BAR("tab_bar", 0, 0, 166, 30),

    YES("icons10x10", 2, 2, 18, 18),
    YES_DOWN("icons10x10", 2, 24, 18, 18),

    REFRESH("icons10x10", 22, 2, 18, 18),
    REFRESH_DOWN("icons10x10", 22, 24, 18, 18),

    TOOL("icons10x10", 42, 2, 18, 18),
    TOOL_DOWN("icons10x10", 42, 24, 18, 18),
    ;

    public static final int FONT_COLOR = 0x575F7A;

    public final ResourceLocation location;
    public final int width;
    public final int height;
    public final int startX;
    public final int startY;

    ControlCraftGuiTextures(String location, int width, int height) {
        this(location, 0, 0, width, height);
    }

    ControlCraftGuiTextures(int startX, int startY) {
        this("icons", startX * 16, startY * 16, 16, 16);
    }

    ControlCraftGuiTextures(String location, int startX, int startY, int width, int height) {
        this(ControlCraft.MODID, location, startX, startY, width, height);
    }

    ControlCraftGuiTextures(String namespace, String location, int startX, int startY, int width, int height) {
        this.location = new ResourceLocation(namespace, "textures/gui/" + location + ".png");
        this.width = width;
        this.height = height;
        this.startX = startX;
        this.startY = startY;
    }

    @OnlyIn(Dist.CLIENT)
    public void bind() {
        RenderSystem.setShaderTexture(0, location);
    }

    @OnlyIn(Dist.CLIENT)
    public void render(GuiGraphics graphics, int x, int y) {
        graphics.blit(location, x, y, startX, startY, width, height);
    }

    @OnlyIn(Dist.CLIENT)
    public void render(GuiGraphics graphics, int x, int y, Color c) {
        bind();
        UIRenderHelper.drawColoredTexture(graphics, c, x, y, startX, startY, width, height);
    }

    @Override
    public int width() {
        return width;
    }

    @Override
    public int height() {
        return height;
    }
}
