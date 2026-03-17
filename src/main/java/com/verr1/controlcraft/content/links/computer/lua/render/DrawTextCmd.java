package com.verr1.controlcraft.content.links.computer.lua.render;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.renderer.LightTexture;
import net.minecraft.client.renderer.MultiBufferSource;

public record DrawTextCmd(String text, float x, float y, float scale, int rgba) implements RenderCmd {

    @Override
    public void execute(PoseStack ms, MultiBufferSource bufferSource, int light, int overlay, float partialTick) {
        Font font = Minecraft.getInstance().font;

        ms.pushPose();
        ms.translate(x, y, 0.001f);
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
                Font.DisplayMode.SEE_THROUGH,
                0, // background color
                LightTexture.FULL_BRIGHT
        );

        ms.popPose();
    }
}
