package com.verr1.controlcraft.content.links.computer.lua.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import org.joml.Matrix4f;

public record DrawLineCmd(float x0, float y0, float x1, float y1, float thickness, int rgba) implements RenderCmd {

    private static final float EPSILON = 1.0e-6f;

    @Override
    public void execute(PoseStack ms, MultiBufferSource bufferSource, int light, int overlay, float partialTick) {
        float dx = x1 - x0;
        float dy = y1 - y0;
        float length = (float) Math.sqrt(dx * dx + dy * dy);
        float half = Math.max(thickness, 0.0f) * 0.5f;

        VertexConsumer vertexConsumer = bufferSource.getBuffer(RenderType.gui());
        Matrix4f matrix4f = ms.last().pose();

        if (length < EPSILON) {
            if (half <= 0.0f) {
                return;
            }
            RenderShapeEmitter.emitAxisAlignedQuad(
                    vertexConsumer,
                    matrix4f,
                    x0 - half,
                    y0 - half,
                    x0 + half,
                    y0 + half,
                    rgba
            );
            return;
        }

        float nx = -dy / length * half;
        float ny = dx / length * half;

        RenderShapeEmitter.emitQuad(
                vertexConsumer,
                matrix4f,
                x0 - nx, y0 - ny,
                x0 + nx, y0 + ny,
                x1 + nx, y1 + ny,
                x1 - nx, y1 - ny,
                rgba
        );
    }
}
