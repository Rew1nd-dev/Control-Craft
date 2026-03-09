package com.verr1.controlcraft.content.links.screen_base.lua.render;

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

        vertexConsumer.vertex(matrix4f, x, y, 0.0F).color(r, g, b, a).endVertex();
        vertexConsumer.vertex(matrix4f, x, y + height, 0.0F).color(r, g, b, a).endVertex();
        vertexConsumer.vertex(matrix4f, x + width, y + height, 0.0F).color(r, g, b, a).endVertex();
        vertexConsumer.vertex(matrix4f, x + width, y, 0.0F).color(r, g, b, a).endVertex();
    }
}
