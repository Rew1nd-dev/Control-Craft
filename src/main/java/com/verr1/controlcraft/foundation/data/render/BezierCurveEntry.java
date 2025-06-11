package com.verr1.controlcraft.foundation.data.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import com.simibubi.create.foundation.render.SuperRenderTypeBuffer;
import com.verr1.controlcraft.utils.BezierCurve;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.world.phys.Vec3;
import org.joml.Matrix4f;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.ArrayList;
import java.util.List;

public class BezierCurveEntry implements RenderableOutline{

    private final Vector3dc start;
    private final Vector3dc end;
    private final Vector3dc startDirection;
    private final Vector3dc endDirection;
    private final int segments;

    private final float width;

    private final List<Vector3d> points;

    List<Vector3dc> tangents = new ArrayList<>();
    List<Vector3dc> normals = new ArrayList<>();
    List<Vector3dc> binormals = new ArrayList<>();
    List<Vector3dc[]> squareVertices = new ArrayList<>();

    public BezierCurveEntry(
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

    public void tick(){

    }

    private void createVertices() {
        if (points.size() < 2) return;
        // 1. 计算切线
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

        // 2. 计算法线和副法线

        Vector3dc up = new Vector3d(0, 1, 0);
        for (Vector3dc tangent : tangents) {
            Vector3dc normal = tangent.cross(up, new Vector3d()).normalize();
            if (normal.lengthSquared() < 0.01) {
                normal = tangent.cross(new Vector3d(1, 0, 0), new Vector3d()).normalize();
                if (normal.lengthSquared() < 0.01) {
                    normal = tangent.cross(new Vector3d(0, 0, 1), new Vector3d()).normalize();
                }
            }
            Vector3dc binormal = tangent.cross(normal, new Vector3d()).normalize(); // 副法线
            normals.add(normal);
            binormals.add(binormal);
        }

        // 3. 计算正方形截面的四个顶点

        for (int i = 0; i < points.size(); i++) {
            Vector3dc point = points.get(i);
            Vector3dc normal = normals.get(i);
            Vector3dc binormal = binormals.get(i);
            float halfWidth = width / 2;
            Vector3d[] vertices = new Vector3d[4];
            vertices[0] = point.add(normal.mul(halfWidth, new Vector3d()), new Vector3d()).add(binormal.mul(halfWidth, new Vector3d()));   // 右上
            vertices[1] = point.add(normal.mul(halfWidth, new Vector3d()), new Vector3d()).add(binormal.mul(-halfWidth, new Vector3d()));  // 右下
            vertices[2] = point.add(normal.mul(-halfWidth, new Vector3d()), new Vector3d()).add(binormal.mul(-halfWidth, new Vector3d())); // 左下
            vertices[3] = point.add(normal.mul(-halfWidth, new Vector3d()), new Vector3d()).add(binormal.mul(halfWidth, new Vector3d()));  // 左上
            squareVertices.add(vertices);
        }
    }


    public void render(PoseStack ms, SuperRenderTypeBuffer buffer, Vec3 camera, float pt){
        ms.pushPose();
        ms.translate(-camera.x, -camera.y, -camera.z); // 相机坐标系转换

        renderInto(
                buffer.getBuffer(RenderType.debugFilledBox()), //RenderTypes.getOutlineTranslucent(AllSpecialTextures.BLANK.getLocation()
                ms.last().pose(),
                0xFF00FFFF // 颜色：青色
        );

        ms.popPose();

    }

    public void renderInto(
            VertexConsumer consumer,
            Matrix4f matrix,
            int color
    ){
        float r = (color >> 16 & 255) / 255f;
        float g = (color >> 8 & 255) / 255f;
        float b = (color & 255) / 255f;

        for (int i = 0; i < points.size() - 1; i++) {
            Vector3dc[] curr = squareVertices.get(i);
            Vector3dc[] next = squareVertices.get(i + 1);

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

    private static void addVertex(VertexConsumer consumer, Matrix4f matrix, Vector3dc pos, float r, float g, float b) {
        consumer.vertex(matrix, (float) pos.x(), (float) pos.y(), (float) pos.z())
                .color(r, g, b, 1.0f)
                .endVertex();
    }

}
