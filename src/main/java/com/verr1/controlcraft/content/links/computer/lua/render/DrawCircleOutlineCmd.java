package com.verr1.controlcraft.content.links.computer.lua.render;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.renderer.MultiBufferSource;

public record DrawCircleOutlineCmd(float cx, float cy, float radius, float thickness, int segments, int rgba) implements RenderCmd {

    @Override
    public void execute(PoseStack ms, MultiBufferSource bufferSource, int light, int overlay, float partialTick) {
        if (radius <= 0.0f || thickness <= 0.0f) {
            return;
        }

        int clampedSegments = Math.max(8, Math.min(256, segments));
        for (int i = 0; i < clampedSegments; i++) {
            double angle0 = Math.PI * 2.0 * i / clampedSegments;
            double angle1 = Math.PI * 2.0 * (i + 1) / clampedSegments;
            float x0 = cx + (float) (Math.cos(angle0) * radius);
            float y0 = cy + (float) (Math.sin(angle0) * radius);
            float x1 = cx + (float) (Math.cos(angle1) * radius);
            float y1 = cy + (float) (Math.sin(angle1) * radius);
            new DrawLineCmd(x0, y0, x1, y1, thickness, rgba).execute(ms, bufferSource, light, overlay, partialTick);
        }
    }
}