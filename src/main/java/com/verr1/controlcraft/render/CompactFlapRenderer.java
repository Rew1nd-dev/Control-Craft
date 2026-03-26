package com.verr1.controlcraft.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import com.simibubi.create.foundation.blockEntity.renderer.SafeBlockEntityRenderer;
import com.simibubi.create.foundation.render.CachedBufferer;
import com.simibubi.create.foundation.render.SuperByteBuffer;
import com.simibubi.create.foundation.utility.AngleHelper;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.blocks.flap.CompactFlapBlock;
import com.verr1.controlcraft.content.blocks.flap.CompactFlapBlockEntity;
import com.verr1.controlcraft.registry.ControlCraftPartialModels;
import com.verr1.controlcraft.utils.MinecraftUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.ItemBlockRenderTypes;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.blockentity.BlockEntityRendererProvider;
import net.minecraft.client.renderer.texture.OverlayTexture;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.world.inventory.InventoryMenu;
import net.minecraft.core.Direction;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.Vec3;
import org.joml.Matrix3f;
import org.joml.Matrix4f;
import org.joml.Quaternionf;

public class CompactFlapRenderer extends SafeBlockEntityRenderer<CompactFlapBlockEntity> {
    private static final float FLAP_MIN_Y = 7f / 16f;
    private static final float FLAP_MAX_Y = 9f / 16f;
    private static final float FULL_TEXTURE_UNITS = 16f;
    private static final float DEFAULT_VISIBLE_DEPTH_UNITS = 14f;

    public CompactFlapRenderer(BlockEntityRendererProvider.Context context) {
    }

    @Override
    protected void renderSafe(CompactFlapBlockEntity be, float partialTicks, PoseStack ms, MultiBufferSource bufferSource, int light, int overlay) {
        float angle = be.getClientAnimatedAngle().getValue(partialTicks);
        float tilt = be.getClientAnimatedTilt().getValue(partialTicks);
        Direction facing = be.getDirection();
        boolean invisibleBase = MinecraftUtils.getValueOptional(be.getBlockState(), CompactFlapBlock.INVISIBLE).orElse(false);

        Direction offsetDirection = be.clientRenderVertical();
        double offsetDistance = be.clientRenderOffset();
        Vec3 renderOffset = new Vec3(offsetDirection.getStepX(), offsetDirection.getStepY(), offsetDirection.getStepZ()).scale(offsetDistance);

        BlockState renderMaterial = be.renderMaterial();
        if (!be.hasRenderMaterial()) {
            renderPartialModel(be, ms, bufferSource, light, facing, invisibleBase, renderOffset, angle, tilt);
            return;
        }

        TextureAtlasSprite sprite = getSprite(renderMaterial, be.hasRenderMaterial());
        int tint = getTint(be, renderMaterial, be.hasRenderMaterial());
        RenderType renderType = be.hasRenderMaterial() ? ItemBlockRenderTypes.getChunkRenderType(renderMaterial) : RenderType.solid();
        VertexConsumer consumer = bufferSource.getBuffer(renderType);

        float widthUnits = (float) (FULL_TEXTURE_UNITS * be.clientFlapWidth());
        float depthUnits = invisibleBase ? FULL_TEXTURE_UNITS : DEFAULT_VISIBLE_DEPTH_UNITS;

        float minX = (FULL_TEXTURE_UNITS - widthUnits) / 2f / FULL_TEXTURE_UNITS;
        float maxX = (FULL_TEXTURE_UNITS + widthUnits) / 2f / FULL_TEXTURE_UNITS;
        float minZ = invisibleBase ? 0f : 5f / 16f;
        float maxZ = minZ + depthUnits / FULL_TEXTURE_UNITS;

        float widthCropMin = (FULL_TEXTURE_UNITS - widthUnits) / 2f / FULL_TEXTURE_UNITS;
        float widthCropMax = 1f - widthCropMin;
        float depthCropMin = (FULL_TEXTURE_UNITS - depthUnits) / 2f / FULL_TEXTURE_UNITS;
        float depthCropMax = 1f - depthCropMin;

        ms.pushPose();
        ms.translate(renderOffset.x, renderOffset.y, renderOffset.z);
        rotateCentered(ms, be.leftDirection(), tilt);
        rotateCentered(ms, facing, angle);
        orientModelToFacing(ms, facing);

        renderBox(
            ms,
            consumer,
            sprite,
            light,
            overlay,
            tint,
            minX,
            FLAP_MIN_Y,
            minZ,
            maxX,
            FLAP_MAX_Y,
            maxZ,
            widthCropMin,
            widthCropMax,
            depthCropMin,
            depthCropMax
        );
        ms.popPose();
    }

    private static void renderPartialModel(
        CompactFlapBlockEntity be,
        PoseStack ms,
        MultiBufferSource bufferSource,
        int light,
        Direction facing,
        boolean invisibleBase,
        Vec3 renderOffset,
        float angle,
        float tilt
    ) {
        BlockState state = be.getBlockState();
        VertexConsumer solid = bufferSource.getBuffer(RenderType.solid());
        float widthScale = (float) be.clientFlapWidth();
        SuperByteBuffer flapBuffer = CachedBufferer.partialFacing(
            invisibleBase ? ControlCraftPartialModels.WING_CONTROLLER_TOP_WIDER : ControlCraftPartialModels.WING_CONTROLLER_TOP,
            state
        );

        float wx = facing.getAxis() == Direction.Axis.X ? 1 : widthScale;
        float wz = facing.getAxis() == Direction.Axis.X ? widthScale : 1;

        flapBuffer
            .translate(renderOffset)

            .rotateCentered(be.leftDirection(), (float) Math.toRadians(tilt))
            .rotateCentered(facing, (float) Math.toRadians(angle))
            .centre()
            .scale(wx, 1f, wz)
            .unCentre()
            .light(light)
            .renderInto(ms, solid);
    }

    private static void orientModelToFacing(PoseStack ms, Direction facing) {
        float horizontal = AngleHelper.horizontalAngle(facing);
        float vertical = AngleHelper.verticalAngle(facing);

        rotateCentered(ms, Direction.UP, horizontal);
        rotateCentered(ms, Direction.EAST, vertical);
    }

    private static TextureAtlasSprite getSprite(BlockState material, boolean useMaterial) {
        Minecraft minecraft = Minecraft.getInstance();
        if (useMaterial) {
            return minecraft.getBlockRenderer()
                .getBlockModelShaper()
                .getBlockModel(material)
                .getParticleIcon();
        }
        return minecraft.getTextureAtlas(InventoryMenu.BLOCK_ATLAS)
            .apply(ControlCraft.asResource("block/compact_flap/flap_rotation"));
    }

    private static int getTint(CompactFlapBlockEntity be, BlockState material, boolean useMaterial) {
        if (!useMaterial || be.getLevel() == null) {
            return 0xFFFFFF;
        }

        int tint = Minecraft.getInstance().getBlockColors().getColor(material, be.getLevel(), be.getBlockPos(), 0);
        return tint == -1 ? 0xFFFFFF : tint;
    }

    private static void rotateCentered(PoseStack ms, Direction axis, float angleDegrees) {
        if (angleDegrees == 0f) {
            return;
        }

        Quaternionf rotation = new Quaternionf().fromAxisAngleDeg(
            axis.getStepX(),
            axis.getStepY(),
            axis.getStepZ(),
            angleDegrees
        );
        rotateCentered(ms, rotation);
    }

    private static void rotateCentered(PoseStack ms, Quaternionf rotation) {
        ms.translate(0.5f, 0.5f, 0.5f);
        ms.mulPose(rotation);
        ms.translate(-0.5f, -0.5f, -0.5f);
    }

    private static void renderBox(
        PoseStack ms,
        VertexConsumer consumer,
        TextureAtlasSprite sprite,
        int light,
        int overlay,
        int tint,
        float minX,
        float minY,
        float minZ,
        float maxX,
        float maxY,
        float maxZ,
        float widthCropMin,
        float widthCropMax,
        float depthCropMin,
        float depthCropMax
    ) {
        Matrix4f pose = ms.last().pose();
        Matrix3f normal = ms.last().normal();

        float uWidthMin = lerpU(sprite, widthCropMin);
        float uWidthMax = lerpU(sprite, widthCropMax);
        float uDepthMin = lerpU(sprite, depthCropMin);
        float uDepthMax = lerpU(sprite, depthCropMax);
        float vDepthMin = lerpV(sprite, depthCropMin);
        float vDepthMax = lerpV(sprite, depthCropMax);
        float vHeightMin = lerpV(sprite, FLAP_MIN_Y);
        float vHeightMax = lerpV(sprite, FLAP_MAX_Y);

        float r = ((tint >> 16) & 0xFF) / 255f;
        float g = ((tint >> 8) & 0xFF) / 255f;
        float b = (tint & 0xFF) / 255f;

        putFace(consumer, pose, normal, light, overlay, r, g, b, 0, -1, 0,
            minX, minY, maxZ, uWidthMin, vDepthMax,
            maxX, minY, maxZ, uWidthMax, vDepthMax,
            maxX, minY, minZ, uWidthMax, vDepthMin,
            minX, minY, minZ, uWidthMin, vDepthMin);

        putFace(consumer, pose, normal, light, overlay, r, g, b, 0, 1, 0,
            minX, maxY, minZ, uWidthMin, vDepthMin,
            maxX, maxY, minZ, uWidthMax, vDepthMin,
            maxX, maxY, maxZ, uWidthMax, vDepthMax,
            minX, maxY, maxZ, uWidthMin, vDepthMax);

        putFace(consumer, pose, normal, light, overlay, r, g, b, 0, 0, -1,
            maxX, maxY, minZ, uWidthMax, vHeightMin,
            minX, maxY, minZ, uWidthMin, vHeightMin,
            minX, minY, minZ, uWidthMin, vHeightMax,
            maxX, minY, minZ, uWidthMax, vHeightMax);

        putFace(consumer, pose, normal, light, overlay, r, g, b, 0, 0, 1,
            minX, maxY, maxZ, uWidthMin, vHeightMin,
            maxX, maxY, maxZ, uWidthMax, vHeightMin,
            maxX, minY, maxZ, uWidthMax, vHeightMax,
            minX, minY, maxZ, uWidthMin, vHeightMax);

        putFace(consumer, pose, normal, light, overlay, r, g, b, -1, 0, 0,
            minX, maxY, minZ, uDepthMin, vHeightMin,
            minX, maxY, maxZ, uDepthMax, vHeightMin,
            minX, minY, maxZ, uDepthMax, vHeightMax,
            minX, minY, minZ, uDepthMin, vHeightMax);

        putFace(consumer, pose, normal, light, overlay, r, g, b, 1, 0, 0,
            maxX, maxY, maxZ, uDepthMax, vHeightMin,
            maxX, maxY, minZ, uDepthMin, vHeightMin,
            maxX, minY, minZ, uDepthMin, vHeightMax,
            maxX, minY, maxZ, uDepthMax, vHeightMax);
    }

    private static void putFace(
        VertexConsumer consumer,
        Matrix4f pose,
        Matrix3f normal,
        int light,
        int overlay,
        float r,
        float g,
        float b,
        float nx,
        float ny,
        float nz,
        float x0, float y0, float z0, float u0, float v0,
        float x1, float y1, float z1, float u1, float v1,
        float x2, float y2, float z2, float u2, float v2,
        float x3, float y3, float z3, float u3, float v3
    ) {
        putVertex(consumer, pose, normal, x0, y0, z0, u0, v0, light, overlay, r, g, b, nx, ny, nz);
        putVertex(consumer, pose, normal, x1, y1, z1, u1, v1, light, overlay, r, g, b, nx, ny, nz);
        putVertex(consumer, pose, normal, x2, y2, z2, u2, v2, light, overlay, r, g, b, nx, ny, nz);
        putVertex(consumer, pose, normal, x3, y3, z3, u3, v3, light, overlay, r, g, b, nx, ny, nz);

        putVertex(consumer, pose, normal, x3, y3, z3, u3, v3, light, overlay, r, g, b, -nx, -ny, -nz);
        putVertex(consumer, pose, normal, x2, y2, z2, u2, v2, light, overlay, r, g, b, -nx, -ny, -nz);
        putVertex(consumer, pose, normal, x1, y1, z1, u1, v1, light, overlay, r, g, b, -nx, -ny, -nz);
        putVertex(consumer, pose, normal, x0, y0, z0, u0, v0, light, overlay, r, g, b, -nx, -ny, -nz);
    }

    private static void putVertex(
        VertexConsumer consumer,
        Matrix4f pose,
        Matrix3f normal,
        float x,
        float y,
        float z,
        float u,
        float v,
        int light,
        int overlay,
        float r,
        float g,
        float b,
        float nx,
        float ny,
        float nz
    ) {
        consumer.vertex(pose, x, y, z)
            .color(r, g, b, 1f)
            .uv(u, v)
            .overlayCoords(overlay)
            .uv2(light)
            .normal(normal, nx, ny, nz)
            .endVertex();
    }

    private static float lerpU(TextureAtlasSprite sprite, float fraction) {
        return sprite.getU0() + (sprite.getU1() - sprite.getU0()) * fraction;
    }

    private static float lerpV(TextureAtlasSprite sprite, float fraction) {
        return sprite.getV0() + (sprite.getV1() - sprite.getV0()) * fraction;
    }

    @Override
    public int getViewDistance() {
        return 1024;
    }
}
