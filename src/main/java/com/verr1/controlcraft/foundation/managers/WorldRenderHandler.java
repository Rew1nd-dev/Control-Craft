package com.verr1.controlcraft.foundation.managers;

import com.mojang.blaze3d.vertex.DefaultVertexFormat;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import com.mojang.blaze3d.vertex.VertexFormat;
import com.verr1.controlcraft.utils.BezierCurve;
import net.minecraft.client.Camera;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderStateShard;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.client.ForgeRenderTypes;
import net.minecraftforge.client.event.RenderLevelStageEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.earlydisplay.ElementShader;
import org.joml.Matrix4f;
import org.joml.Vector3d;

import java.util.ArrayList;
import java.util.List;


public class WorldRenderHandler {
    private static final int CURVE_SEGMENTS = 50;
    private static final float LINE_WIDTH = 0.1f; // 线的厚度（方块单位）


    @SubscribeEvent
    public static void onRenderWorld(RenderLevelStageEvent event) {
        if (event.getStage() != RenderLevelStageEvent.Stage.AFTER_SOLID_BLOCKS) return;

        // 1. 准备渲染资源
        PoseStack poseStack = event.getPoseStack();
        MultiBufferSource.BufferSource bufferSource = Minecraft.getInstance().renderBuffers().bufferSource();
        Camera camera = Minecraft.getInstance().gameRenderer.getMainCamera();
        Vec3 cameraPos = camera.getPosition();

        // 2. 定义控制点（示例）
        Vector3d p0 = new Vector3d(0, 64, 0);
        Vector3d p1 = new Vector3d(10, 80, 5);
        Vector3d p2 = new Vector3d(20, 60, 10);
        Vector3d p3 = new Vector3d(30, 70, 15);

        // 3. 计算曲线点
        List<Vector3d> curvePoints = BezierCurve.calculateCubicBezier(p0, p1, p2, p3, CURVE_SEGMENTS);

        // 4. 开始渲染
        poseStack.pushPose();
        poseStack.translate(-cameraPos.x, -cameraPos.y, -cameraPos.z); // 相机坐标系转换

        // 方案1：渲染为细线（1像素宽）
        // renderThinLine(poseStack, bufferSource, curvePoints, 0xFF00FFFF);

        // 方案2：渲染为有厚度的带状（推荐）
        renderThickLine(
                poseStack,
                bufferSource,
                curvePoints.stream().map(v -> new Vec3(v.x, v.y, v.z)).toList(),
                LINE_WIDTH,
                0xFFFF0000
        );

        poseStack.popPose();
        bufferSource.endBatch(); // 提交批次
    }

    // 方案1：渲染细线（1像素宽）
    private static void renderThinLine(PoseStack poseStack, MultiBufferSource buffer, List<Vec3> points, int color) {
        VertexConsumer consumer = buffer.getBuffer(RenderType.lines());
        Matrix4f matrix = poseStack.last().pose();

        for (int i = 0; i < points.size() - 1; i++) {
            Vec3 start = points.get(i);
            Vec3 end = points.get(i + 1);

            consumer.vertex(matrix, (float) start.x, (float) start.y, (float) start.z)
                    .color(color)
                    .endVertex();
            consumer.vertex(matrix, (float) end.x, (float) end.y, (float) end.z)
                    .color(color)
                    .endVertex();
        }
    }

    private static void renderThickLine(PoseStack poseStack, MultiBufferSource buffer, List<Vec3> points, float width, int color) {
        if (points.size() < 2) return;

        VertexConsumer consumer = buffer.getBuffer(RenderType.debugFilledBox());
        Matrix4f matrix = poseStack.last().pose();
        float r = (color >> 16 & 255) / 255f;
        float g = (color >> 8 & 255) / 255f;
        float b = (color & 255) / 255f;

        // 1. 计算切线
        List<Vec3> tangents = new ArrayList<>();
        for (int i = 0; i < points.size(); i++) {
            Vec3 tangent;
            if (i == 0) {
                tangent = points.get(1).subtract(points.get(0)).normalize();
            } else if (i == points.size() - 1) {
                tangent = points.get(i).subtract(points.get(i - 1)).normalize();
            } else {
                tangent = points.get(i + 1).subtract(points.get(i - 1)).normalize();
            }
            tangents.add(tangent);
        }

        // 2. 计算法线和副法线
        List<Vec3> normals = new ArrayList<>();
        List<Vec3> binormals = new ArrayList<>();
        Vec3 up = new Vec3(0, 1, 0);
        for (Vec3 tangent : tangents) {
            Vec3 normal = tangent.cross(up).normalize();
            if (normal.lengthSqr() < 0.01) {
                normal = tangent.cross(new Vec3(1, 0, 0)).normalize();
                if (normal.lengthSqr() < 0.01) {
                    normal = tangent.cross(new Vec3(0, 0, 1)).normalize();
                }
            }
            Vec3 binormal = tangent.cross(normal).normalize(); // 副法线
            normals.add(normal);
            binormals.add(binormal);
        }

        // 3. 计算正方形截面的四个顶点
        List<Vec3[]> squareVertices = new ArrayList<>();
        for (int i = 0; i < points.size(); i++) {
            Vec3 point = points.get(i);
            Vec3 normal = normals.get(i);
            Vec3 binormal = binormals.get(i);
            float halfWidth = width / 2;
            Vec3[] vertices = new Vec3[4];
            vertices[0] = point.add(normal.scale(halfWidth)).add(binormal.scale(halfWidth));   // 右上
            vertices[1] = point.add(normal.scale(halfWidth)).add(binormal.scale(-halfWidth));  // 右下
            vertices[2] = point.add(normal.scale(-halfWidth)).add(binormal.scale(-halfWidth)); // 左下
            vertices[3] = point.add(normal.scale(-halfWidth)).add(binormal.scale(halfWidth));  // 左上
            squareVertices.add(vertices);
        }

        // 4. 渲染管状结构
        for (int i = 0; i < points.size() - 1; i++) {
            Vec3[] curr = squareVertices.get(i);
            Vec3[] next = squareVertices.get(i + 1);

            // 每个面由两个三角形组成，共4个面
            // 上面（0-1-4-5）
            addVertex(consumer, matrix, curr[0], r, g, b);
            addVertex(consumer, matrix, curr[1], r, g, b);
            addVertex(consumer, matrix, next[0], r, g, b);
            addVertex(consumer, matrix, curr[1], r, g, b);
            addVertex(consumer, matrix, next[1], r, g, b);
            addVertex(consumer, matrix, next[0], r, g, b);

            // 下面（2-3-6-7）
            addVertex(consumer, matrix, curr[2], r, g, b);
            addVertex(consumer, matrix, curr[3], r, g, b);
            addVertex(consumer, matrix, next[2], r, g, b);
            addVertex(consumer, matrix, curr[3], r, g, b);
            addVertex(consumer, matrix, next[3], r, g, b);
            addVertex(consumer, matrix, next[2], r, g, b);

            // 左面（3-0-7-4）
            addVertex(consumer, matrix, curr[3], r, g, b);
            addVertex(consumer, matrix, curr[0], r, g, b);
            addVertex(consumer, matrix, next[3], r, g, b);
            addVertex(consumer, matrix, curr[0], r, g, b);
            addVertex(consumer, matrix, next[0], r, g, b);
            addVertex(consumer, matrix, next[3], r, g, b);

            // 右面（1-2-5-6）
            addVertex(consumer, matrix, curr[1], r, g, b);
            addVertex(consumer, matrix, curr[2], r, g, b);
            addVertex(consumer, matrix, next[1], r, g, b);
            addVertex(consumer, matrix, curr[2], r, g, b);
            addVertex(consumer, matrix, next[2], r, g, b);
            addVertex(consumer, matrix, next[1], r, g, b);
        }
    }

    private static void addVertex(VertexConsumer consumer, Matrix4f matrix, Vec3 pos, float r, float g, float b) {
        consumer.vertex(matrix, (float) pos.x, (float) pos.y, (float) pos.z)
                .color(r, g, b, 1.0f)
                .endVertex();
    }
}
