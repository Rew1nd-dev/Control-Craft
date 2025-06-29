package com.verr1.controlcraft.foundation.data.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import com.simibubi.create.foundation.render.SuperRenderTypeBuffer;
import com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies;
import com.verr1.controlcraft.utils.BezierCurve;
import com.verr1.controlcraft.utils.MinecraftUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.texture.OverlayTexture;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;
import org.joml.*;
import org.valkyrienskies.core.api.ships.ClientShip;
import org.valkyrienskies.core.api.ships.Ship;
import org.valkyrienskies.core.api.ships.properties.ShipTransform;
import org.valkyrienskies.core.impl.game.ships.ShipTransformImpl;

import java.lang.Math;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public class CimulinkWireEntry implements RenderableOutline {
    private final Vector3dc start;
    private final Vector3dc end;

    private final Vector3d start_wc = new Vector3d();
    private final Vector3d end_wc = new Vector3d();

    private final Vector3dc startDirection;
    private final Vector3dc endDirection;

    private final Vector3d startDirection_wc = new Vector3d();
    private final Vector3d endDirection_wc = new Vector3d();

    private final int segments;
    private final float width;
    private final List<Vector3d> offsets = new ArrayList<>();

    private boolean shouldAlwaysRecreate = false;

    // 新增变量
    private float lightStart = 0.3f; // 起点光照，默认全亮
    private float lightEnd = 0.3f;   // 终点光照，默认全亮
    private int flashFrame = -1;     // 闪烁帧计数，-1 表示未启动

    List<Vector3dc> tangents = new ArrayList<>();
    List<Vector3dc> normals = new ArrayList<>();
    List<Vector3dc> binormals = new ArrayList<>();
    List<Vector3dc[]> squareVertices = new ArrayList<>();

    public CimulinkWireEntry(
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
        checkAlways();
        recreate();
    }

    private void clearAll(){
        offsets.clear();
        tangents.clear();
        normals.clear();
        binormals.clear();
        squareVertices.clear();
    }

    public void checkAlways() {
        Ship s0 = ValkyrienSkies.getShipManagingBlock(Minecraft.getInstance().level, BlockPos.containing(toMinecraft(start)));
        Ship s1 = ValkyrienSkies.getShipManagingBlock(Minecraft.getInstance().level, BlockPos.containing(toMinecraft(end)));
        if(s0 == null || s1 == null){
            shouldAlwaysRecreate = !(s0 == null && s1 == null);
            return;
        }
        shouldAlwaysRecreate = s0.getId() != s1.getId();
    }

    public void calculateWorldCoordinate(){
        if(!shouldAlwaysRecreate){
            start_wc.set(start);
            end_wc.set(end);
            startDirection_wc.set(startDirection);
            endDirection_wc.set(endDirection);
            return;
        }
        Matrix4dc transform_start = transformAt(start);
        start_wc.set(transform_start.transformPosition(start, new Vector3d()));
        startDirection_wc.set(transform_start.transformDirection(startDirection, new Vector3d()));


        Matrix4dc transform_end = transformAt(end);

        end_wc.set(transform_end.transformPosition(end, new Vector3d()));
        endDirection_wc.set(transform_end.transformDirection(endDirection, new Vector3d()));
    }

    private void recreate(){
        clearAll();
        calculateWorldCoordinate();
        this.offsets.addAll(BezierCurve.calculateCubicBezier(
                new Vector3d(),
                startDirection_wc,
                end_wc.sub(start_wc, new Vector3d()).add(endDirection_wc, new Vector3d()),
                end_wc.sub(start_wc, new Vector3d()),
                segments
        ));
        createVertices();
    }

    // 设置光照
    public CimulinkWireEntry setLights(float lightStart, float lightEnd) {
        this.lightStart = Math.max(0, Math.min(1, lightStart));
        this.lightEnd = Math.max(0, Math.min(1, lightEnd));
        return this;
    }

    // 启动闪烁
    public CimulinkWireEntry flash() {
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
        if (offsets.size() < 2) return;
        for (int i = 0; i < offsets.size(); i++) {
            Vector3d tangent;
            if (i == 0) {
                tangent = offsets.get(1).sub(offsets.get(0), new Vector3d()).normalize();
            } else if (i == offsets.size() - 1) {
                tangent = offsets.get(i).sub(offsets.get(i - 1), new Vector3d()).normalize();
            } else {
                tangent = offsets.get(i + 1).sub(offsets.get(i - 1), new Vector3d()).normalize();
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

        for (int i = 0; i < offsets.size(); i++) {
            Vector3dc point = offsets.get(i);
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

    public void render(PoseStack ms, MultiBufferSource buffer, Vec3 camera, float pt) {
        ms.pushPose();
        ms.translate(-camera.x, -camera.y, -camera.z);

        if (shouldAlwaysRecreate)recreate();

        float[] lightLevels = new float[segments]; // 存储每个 segment 的光照
        for (int i = 0; i < segments; i++) {
            float t = (float) i / (segments - 1);
            lightLevels[i] = lightStart + (lightEnd - lightStart) * t;
        }

        // When start and end located at different ship, the position is already transformed
        Matrix4dc transform = transformAt(start);
        Matrix4dc renderTransform = new Matrix4d(ms.last().pose());
        Matrix4dc coupled = new Matrix4d(renderTransform).mul(transform, new Matrix4d());

        renderIntoTransformed(
                buffer.getBuffer(RenderType.debugFilledBox()), // buffer.getBuffer(RenderTypes.getOutlineTranslucent(AllSpecialTextures.BLANK.getLocation(), true)), //
                shouldAlwaysRecreate ? renderTransform : coupled,//transform.mul(ms.last().pose(), new Matrix4f()),//
                coupled.transformPosition(start, new Vector3d()),
                0xFF00FFFF, // 默认颜色：青色
                0xFFFF00FF, // 闪烁颜色：紫色
                lightLevels
        );

        ms.popPose();



    }

    public Matrix4dc transformAt(Vector3dc v){
        return  Optional.ofNullable(ValkyrienSkies.getShipManagingBlock(Minecraft.getInstance().level, BlockPos.containing(toMinecraft(v))))
                .map(ship -> (ClientShip)ship)
                .map(ClientShip::getRenderTransform)
                .map(ShipTransform::getShipToWorld)
                .orElse(new Matrix4d());
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

        for (int i = 0; i < offsets.size() - 1; i++) {
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

    public void renderIntoTransformed(
            VertexConsumer consumer,
            Matrix4dc matrix,
            Vector3dc transformedStart,
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

        for (int i = 0; i < offsets.size() - 1; i++) {
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
            addVertex(consumer, matrix.transformDirection(curr[0], new Vector3d()).add(transformedStart, new Vector3d()), lr, lg, lb); // 右上
            addVertex(consumer, matrix.transformDirection(next[0], new Vector3d()).add(transformedStart, new Vector3d()), lr, lg, lb); // 下一个右上
            addVertex(consumer, matrix.transformDirection(next[1], new Vector3d()).add(transformedStart, new Vector3d()), lr, lg, lb); // 下一个右下
            addVertex(consumer, matrix.transformDirection(curr[0], new Vector3d()).add(transformedStart, new Vector3d()), lr, lg, lb); // 右上
            addVertex(consumer, matrix.transformDirection(next[1], new Vector3d()).add(transformedStart, new Vector3d()), lr, lg, lb); // 下一个右下
            addVertex(consumer, matrix.transformDirection(curr[1], new Vector3d()).add(transformedStart, new Vector3d()), lr, lg, lb); // 右下

            // 下面
            addVertex(consumer, matrix.transformDirection(curr[2], new Vector3d()).add(transformedStart, new Vector3d()), lr, lg, lb); // 左下
            addVertex(consumer, matrix.transformDirection(next[2], new Vector3d()).add(transformedStart, new Vector3d()), lr, lg, lb); // 下一个左下
            addVertex(consumer, matrix.transformDirection(next[3], new Vector3d()).add(transformedStart, new Vector3d()), lr, lg, lb); // 下一个左上
            addVertex(consumer, matrix.transformDirection(curr[2], new Vector3d()).add(transformedStart, new Vector3d()), lr, lg, lb); // 左下
            addVertex(consumer, matrix.transformDirection(next[3], new Vector3d()).add(transformedStart, new Vector3d()), lr, lg, lb); // 下一个左上
            addVertex(consumer, matrix.transformDirection(curr[3], new Vector3d()).add(transformedStart, new Vector3d()), lr, lg, lb); // 左上

            // 左面
            addVertex(consumer, matrix.transformDirection(curr[3], new Vector3d()).add(transformedStart, new Vector3d()), lr, lg, lb); // 左上
            addVertex(consumer, matrix.transformDirection(next[3], new Vector3d()).add(transformedStart, new Vector3d()), lr, lg, lb); // 下一个左上
            addVertex(consumer, matrix.transformDirection(next[0], new Vector3d()).add(transformedStart, new Vector3d()), lr, lg, lb); // 下一个右上
            addVertex(consumer, matrix.transformDirection(curr[3], new Vector3d()).add(transformedStart, new Vector3d()), lr, lg, lb); // 左上
            addVertex(consumer, matrix.transformDirection(next[0], new Vector3d()).add(transformedStart, new Vector3d()), lr, lg, lb); // 下一个右上
            addVertex(consumer, matrix.transformDirection(curr[0], new Vector3d()).add(transformedStart, new Vector3d()), lr, lg, lb); // 右上

            // 右面
            addVertex(consumer, matrix.transformDirection(curr[1], new Vector3d()).add(transformedStart, new Vector3d()), lr, lg, lb); // 右下
            addVertex(consumer, matrix.transformDirection(next[1], new Vector3d()).add(transformedStart, new Vector3d()), lr, lg, lb); // 下一个右下
            addVertex(consumer, matrix.transformDirection(next[2], new Vector3d()).add(transformedStart, new Vector3d()), lr, lg, lb); // 下一个左下
            addVertex(consumer, matrix.transformDirection(curr[1], new Vector3d()).add(transformedStart, new Vector3d()), lr, lg, lb); // 右下
            addVertex(consumer, matrix.transformDirection(next[2], new Vector3d()).add(transformedStart, new Vector3d()), lr, lg, lb); // 下一个左下
            addVertex(consumer, matrix.transformDirection(curr[2], new Vector3d()).add(transformedStart, new Vector3d()), lr, lg, lb); // 左下

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

    private static void addVertex(VertexConsumer consumer, Vector3dc pos, float r, float g, float b) {
        consumer.vertex((float) pos.x(), (float) pos.y(), (float) pos.z())
                .color(r, g, b, 1.0f)
                .endVertex();

    }

}
