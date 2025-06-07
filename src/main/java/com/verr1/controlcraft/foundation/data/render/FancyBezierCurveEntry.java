package com.verr1.controlcraft.foundation.data.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import com.simibubi.create.foundation.render.SuperRenderTypeBuffer;
import com.verr1.controlcraft.utils.BezierCurve;
import com.verr1.controlcraft.utils.MinecraftUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.LightLayer;
import net.minecraft.world.phys.Vec3;
import org.joml.Matrix4f;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.ArrayList;
import java.util.List;

public class FancyBezierCurveEntry implements RenderableOutline {
    private final Vector3dc start;
    private final Vector3dc end;
    private final Vector3dc startDirection;
    private final Vector3dc endDirection;
    private final int segments;
    private final float width;
    private final List<Vector3d> points;

    // 新增变量
    private float lightStart = 0.3f; // 起点光照，默认全亮
    private float lightEnd = 0.3f;   // 终点光照，默认全亮
    private int flashFrame = -1;     // 闪烁帧计数，-1 表示未启动

    List<Vector3dc> tangents = new ArrayList<>();
    List<Vector3dc> normals = new ArrayList<>();
    List<Vector3dc> binormals = new ArrayList<>();
    List<Vector3dc[]> squareVertices = new ArrayList<>();

    public FancyBezierCurveEntry(
            Vector3dc start,
            Vector3dc end,
            Vector3dc startDirection,
            Vector3dc endDirection,
            float width,
            int segments
    ) {
        this.start = start;
        this.end = end;
        this.startDirection = startDirection;
        this.endDirection = endDirection;
        this.segments = segments;
        this.width = width;
        this.points = BezierCurve.calculateCubicBezier(
                start,
                start.add(startDirection, new Vector3d()),
                end.add(endDirection, new Vector3d()),
                end,
                segments
        );
        createVertices();
    }

    // 设置光照
    public FancyBezierCurveEntry setLights(float lightStart, float lightEnd) {
        this.lightStart = Math.max(0, Math.min(1, lightStart));
        this.lightEnd = Math.max(0, Math.min(1, lightEnd));
        return this;
    }

    // 启动闪烁
    public FancyBezierCurveEntry flash() {
        if(this.flashFrame != -1)return this;
        this.flashFrame = 0;
        return this;
    }

    // 更新帧
    public void tick() {
        if (flashFrame >= 0) {
            flashFrame++;
            if (flashFrame >= segments) {
                flashFrame = -1; // 结束闪烁
            }
        }
    }

    private void createVertices() {
        if (points.size() < 2) return;
        for (int i = 0; i < points.size(); i++) {
            Vector3d tangent;
            if (i == 0) {
                tangent = points.get(1).sub(points.get(0), new Vector3d()).normalize();
            } else if (i == points.size() - 1) {
                tangent = points.get(i).sub(points.get(i - 1), new Vector3d()).normalize();
            } else {
                tangent = points.get(i + 1).sub(points.get(i - 1), new Vector3d()).normalize();
            }
            tangents.add(tangent);
        }

        Vector3dc up = new Vector3d(0, 1, 0);
        for (Vector3dc tangent : tangents) {
            Vector3dc normal = tangent.cross(up, new Vector3d()).normalize();
            if (normal.lengthSquared() < 0.01) {
                normal = tangent.cross(new Vector3d(1, 0, 0), new Vector3d()).normalize();
                if (normal.lengthSquared() < 0.01) {
                    normal = tangent.cross(new Vector3d(0, 0, 1), new Vector3d()).normalize();
                }
            }
            Vector3dc binormal = tangent.cross(normal, new Vector3d()).normalize();
            normals.add(normal);
            binormals.add(binormal);
        }

        for (int i = 0; i < points.size(); i++) {
            Vector3dc point = points.get(i);
            Vector3dc normal = normals.get(i);
            Vector3dc binormal = binormals.get(i);
            float halfWidth = width / 2;
            Vector3d[] vertices = new Vector3d[4];
            vertices[0] = point.add(normal.mul(halfWidth, new Vector3d()), new Vector3d()).add(binormal.mul(halfWidth, new Vector3d()));
            vertices[1] = point.add(normal.mul(halfWidth, new Vector3d()), new Vector3d()).add(binormal.mul(-halfWidth, new Vector3d()));
            vertices[2] = point.add(normal.mul(-halfWidth, new Vector3d()), new Vector3d()).add(binormal.mul(-halfWidth, new Vector3d()));
            vertices[3] = point.add(normal.mul(-halfWidth, new Vector3d()), new Vector3d()).add(binormal.mul(halfWidth, new Vector3d()));
            squareVertices.add(vertices);
        }
    }

    public void render(PoseStack ms, SuperRenderTypeBuffer buffer, Vec3 camera) {
        ms.pushPose();
        ms.translate(-camera.x, -camera.y, -camera.z);

        Minecraft mc = Minecraft.getInstance();
        Level level = mc.level;
        float[] lightLevels = new float[segments]; // 存储每个 segment 的光照
        boolean useInterpolated = true;
        if (level != null && !useInterpolated) {
            // 获取实际世界光照
            for (int i = 0; i < points.size() - 1; i++) {
                Vector3dc p1 = points.get(i);
                Vector3dc p2 = points.get(i + 1);
                Vector3dc mid = p1.add(p2, new Vector3d()).mul(0.5);
                BlockPos pos = new BlockPos((int) mid.x(), (int) mid.y(), (int) mid.z());
                int light = MinecraftUtils.getPerceivedLightLevel(pos);
                lightLevels[i] = light / 15.0f; // 转换为 0-1 范围
            }
        } else {
            // 使用插值光照
            for (int i = 0; i < segments; i++) {
                float t = (float) i / (segments - 1);
                lightLevels[i] = lightStart + (lightEnd - lightStart) * t;
            }
        }

        renderInto(
                buffer.getBuffer(RenderType.debugFilledBox()),
                ms.last().pose(),
                0xFF00FFFF, // 默认颜色：青色
                0xFFFF00FF, // 闪烁颜色：紫色
                lightLevels
        );

        ms.popPose();
    }

    public void renderInto(
            VertexConsumer consumer,
            Matrix4f matrix,
            int color,
            int flashColor,
            float[] lightLevels
    ) {
        float r = (color >> 16 & 255) / 255f;
        float g = (color >> 8 & 255) / 255f;
        float b = (color & 255) / 255f;
        float fr = (flashColor >> 16 & 255) / 255f;
        float fg = (flashColor >> 8 & 255) / 255f;
        float fb = (flashColor & 255) / 255f;

        for (int i = 0; i < points.size() - 1; i++) {
            Vector3dc[] curr = squareVertices.get(i);
            Vector3dc[] next = squareVertices.get(i + 1);

            // 判断是否为闪烁 segment
            boolean isFlashing = (flashFrame == i);
            float lr = isFlashing ? fr : r;
            float lg = isFlashing ? fg : g;
            float lb = isFlashing ? fb : b;

            // 应用光照
            float light = lightLevels[i];
            lr *= light;
            lg *= light;
            lb *= light;

            // 上面
            addVertex(consumer, matrix, curr[0], lr, lg, lb); // 右上
            addVertex(consumer, matrix, next[0], lr, lg, lb); // 下一个右上
            addVertex(consumer, matrix, next[1], lr, lg, lb); // 下一个右下
            addVertex(consumer, matrix, curr[0], lr, lg, lb); // 右上
            addVertex(consumer, matrix, next[1], lr, lg, lb); // 下一个右下
            addVertex(consumer, matrix, curr[1], lr, lg, lb); // 右下

            // 下面
            addVertex(consumer, matrix, curr[2], lr, lg, lb); // 左下
            addVertex(consumer, matrix, next[2], lr, lg, lb); // 下一个左下
            addVertex(consumer, matrix, next[3], lr, lg, lb); // 下一个左上
            addVertex(consumer, matrix, curr[2], lr, lg, lb); // 左下
            addVertex(consumer, matrix, next[3], lr, lg, lb); // 下一个左上
            addVertex(consumer, matrix, curr[3], lr, lg, lb); // 左上

            // 左面
            addVertex(consumer, matrix, curr[3], lr, lg, lb); // 左上
            addVertex(consumer, matrix, next[3], lr, lg, lb); // 下一个左上
            addVertex(consumer, matrix, next[0], lr, lg, lb); // 下一个右上
            addVertex(consumer, matrix, curr[3], lr, lg, lb); // 左上
            addVertex(consumer, matrix, next[0], lr, lg, lb); // 下一个右上
            addVertex(consumer, matrix, curr[0], lr, lg, lb); // 右上

            // 右面
            addVertex(consumer, matrix, curr[1], lr, lg, lb); // 右下
            addVertex(consumer, matrix, next[1], lr, lg, lb); // 下一个右下
            addVertex(consumer, matrix, next[2], lr, lg, lb); // 下一个左下
            addVertex(consumer, matrix, curr[1], lr, lg, lb); // 右下
            addVertex(consumer, matrix, next[2], lr, lg, lb); // 下一个左下
            addVertex(consumer, matrix, curr[2], lr, lg, lb); // 左下

/*
*



* */

        }
    }

    private static void addVertex(VertexConsumer consumer, Matrix4f matrix, Vector3dc pos, float r, float g, float b) {
        consumer.vertex(matrix, (float) pos.x(), (float) pos.y(), (float) pos.z())
                .color(r, g, b, 1.0f)
                .endVertex();
    }
}
