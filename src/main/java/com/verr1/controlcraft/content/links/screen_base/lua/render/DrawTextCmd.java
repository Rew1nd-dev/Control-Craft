package com.verr1.controlcraft.content.links.screen_base.lua.render;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.renderer.MultiBufferSource;

public record DrawTextCmd(String text, float x, float y, float scale, int rgba) implements RenderCmd {

    @Override
    public void execute(PoseStack ms, MultiBufferSource bufferSource, int light, int overlay, float partialTick) {
        Font font = Minecraft.getInstance().font;

        ms.pushPose();
        ms.translate(x, y, 0);
        ms.scale(scale, scale, 1.0f);

        // 绘制阴影文字，颜色值需要是 ARGB 格式 (Minecraft 标准)
        font.drawInBatch(
                text,
                0,
                0,
                rgba,
                false, // shadow
                ms.last().pose(),
                bufferSource,
                Font.DisplayMode.NORMAL,
                0, // background color
                light // block light packed
        );

        ms.popPose();
    }
}
