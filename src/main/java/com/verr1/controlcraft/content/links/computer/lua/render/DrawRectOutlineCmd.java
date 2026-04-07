package com.verr1.controlcraft.content.links.computer.lua.render;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.renderer.MultiBufferSource;

public record DrawRectOutlineCmd(float x, float y, float width, float height, float thickness, int rgba) implements RenderCmd {

    @Override
    public void execute(PoseStack ms, MultiBufferSource bufferSource, int light, int overlay, float partialTick) {
        float t = Math.max(0.0f, thickness);
        if (t <= 0.0f || width == 0.0f || height == 0.0f) {
            return;
        }

        new DrawLineCmd(x, y, x + width, y, t, rgba).execute(ms, bufferSource, light, overlay, partialTick);
        new DrawLineCmd(x + width, y, x + width, y + height, t, rgba).execute(ms, bufferSource, light, overlay, partialTick);
        new DrawLineCmd(x + width, y + height, x, y + height, t, rgba).execute(ms, bufferSource, light, overlay, partialTick);
        new DrawLineCmd(x, y + height, x, y, t, rgba).execute(ms, bufferSource, light, overlay, partialTick);
    }
}
