package com.verr1.controlcraft.content.links.computer.lua.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import org.joml.Matrix4f;

public record DrawCircleCmd(float cx, float cy, float radius, int segments, int rgba) implements RenderCmd {

    @Override
    public void execute(PoseStack ms, MultiBufferSource bufferSource, int light, int overlay, float partialTick) {
        if (radius <= 0.0f) {
            return;
        }

        VertexConsumer vertexConsumer = bufferSource.getBuffer(RenderType.gui());
        Matrix4f matrix4f = ms.last().pose();
        int clampedSegments = Math.max(8, Math.min(256, segments));

        for (int i = 0; i < clampedSegments; i++) {
            double angle0 = Math.PI * 2.0 * i / clampedSegments;
            double angle1 = Math.PI * 2.0 * (i + 1) / clampedSegments;
            float x0 = cx + (float) (Math.cos(angle0) * radius);
            float y0 = cy + (float) (Math.sin(angle0) * radius);
            float x1 = cx + (float) (Math.cos(angle1) * radius);
            float y1 = cy + (float) (Math.sin(angle1) * radius);
            RenderShapeEmitter.emitTriangle(vertexConsumer, matrix4f, cx, cy, x0, y0, x1, y1, rgba);
        }
    }
}