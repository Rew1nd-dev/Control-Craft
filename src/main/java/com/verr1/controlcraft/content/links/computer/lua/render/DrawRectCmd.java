package com.verr1.controlcraft.content.links.computer.lua.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import org.joml.Matrix4f;

public record DrawRectCmd(float x, float y, float width, float height, int rgba) implements RenderCmd {

    @Override
    public void execute(PoseStack ms, MultiBufferSource bufferSource, int light, int overlay, float partialTick) {
        int a = (rgba >> 24) & 255;
        int r = (rgba >> 16) & 255;
        int g = (rgba >> 8) & 255;
        int b = rgba & 255;

        VertexConsumer vertexConsumer = bufferSource.getBuffer(RenderType.gui());
        Matrix4f matrix4f = ms.last().pose();

        emitQuad(vertexConsumer, matrix4f, x, y, x + width, y + height, r, g, b, a);
    }

    private static void emitQuad(
            VertexConsumer vertexConsumer,
            Matrix4f matrix4f,
            float x0,
            float y0,
            float x1,
            float y1,
            int r,
            int g,
            int b,
            int a
    ) {
        vertexConsumer.vertex(matrix4f, x0, y0, 0.0F).color(r, g, b, a).endVertex();
        vertexConsumer.vertex(matrix4f, x0, y1, 0.0F).color(r, g, b, a).endVertex();
        vertexConsumer.vertex(matrix4f, x1, y1, 0.0F).color(r, g, b, a).endVertex();
        vertexConsumer.vertex(matrix4f, x1, y0, 0.0F).color(r, g, b, a).endVertex();
    }
}
