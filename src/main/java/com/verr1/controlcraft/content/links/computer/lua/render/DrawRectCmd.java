package com.verr1.controlcraft.content.links.computer.lua.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import org.joml.Matrix4f;

public record DrawRectCmd(float x, float y, float width, float height, int rgba) implements RenderCmd {

    @Override
    public void execute(PoseStack ms, MultiBufferSource bufferSource, int light, int overlay, float partialTick) {
        VertexConsumer vertexConsumer = bufferSource.getBuffer(RenderType.gui());
        Matrix4f matrix4f = ms.last().pose();
        RenderShapeEmitter.emitAxisAlignedQuad(vertexConsumer, matrix4f, x, y, x + width, y + height, rgba);
    }
}