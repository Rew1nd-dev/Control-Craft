package com.verr1.controlcraft.content.links.computer.lua.render;

import com.mojang.blaze3d.vertex.VertexConsumer;
import org.joml.Matrix4f;

final class RenderShapeEmitter {

    private RenderShapeEmitter() {
    }

    static int alpha(int rgba) {
        return (rgba >> 24) & 255;
    }

    static int red(int rgba) {
        return (rgba >> 16) & 255;
    }

    static int green(int rgba) {
        return (rgba >> 8) & 255;
    }

    static int blue(int rgba) {
        return rgba & 255;
    }

    static void emitAxisAlignedQuad(
            VertexConsumer vertexConsumer,
            Matrix4f matrix4f,
            float x0,
            float y0,
            float x1,
            float y1,
            int rgba
    ) {
        emitQuad(
                vertexConsumer,
                matrix4f,
                x0, y0,
                x0, y1,
                x1, y1,
                x1, y0,
                rgba
        );
    }

    static void emitQuad(
            VertexConsumer vertexConsumer,
            Matrix4f matrix4f,
            float x0,
            float y0,
            float x1,
            float y1,
            float x2,
            float y2,
            float x3,
            float y3,
            int rgba
    ) {
        int a = alpha(rgba);
        int r = red(rgba);
        int g = green(rgba);
        int b = blue(rgba);

        vertexConsumer.vertex(matrix4f, x0, y0, 0.0F).color(r, g, b, a).endVertex();
        vertexConsumer.vertex(matrix4f, x1, y1, 0.0F).color(r, g, b, a).endVertex();
        vertexConsumer.vertex(matrix4f, x2, y2, 0.0F).color(r, g, b, a).endVertex();
        vertexConsumer.vertex(matrix4f, x3, y3, 0.0F).color(r, g, b, a).endVertex();
    }

    static void emitTriangle(
            VertexConsumer vertexConsumer,
            Matrix4f matrix4f,
            float x0,
            float y0,
            float x1,
            float y1,
            float x2,
            float y2,
            int rgba
    ) {
        // RenderType.gui() batches quads, so we use a degenerate quad to represent one triangle.
        emitQuad(vertexConsumer, matrix4f, x0, y0, x1, y1, x2, y2, x0, y0, rgba);
    }
}