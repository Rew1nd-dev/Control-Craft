package com.verr1.controlcraft.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import com.mojang.math.Axis;
import com.simibubi.create.foundation.blockEntity.renderer.SafeBlockEntityRenderer;
import com.simibubi.create.foundation.render.SuperByteBuffer;
import com.simibubi.create.foundation.utility.AnimationTickHolder;
import com.verr1.controlcraft.content.blocks.jet.JetRudderBlockEntity;
import com.verr1.controlcraft.registry.ControlCraftPartialModels;
import com.verr1.controlcraft.utils.MathUtils;
import net.minecraft.client.renderer.LightTexture;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.blockentity.BlockEntityRendererProvider;
import net.minecraft.core.Direction;
import net.minecraft.util.Mth;
import org.joml.Matrix4f;
import org.joml.Quaternionf;
import org.joml.Vector3dc;

public class JetRudderRenderer extends SafeBlockEntityRenderer<JetRudderBlockEntity> {
    // Tunables for jet flame transform.
    // Offset unit is blocks; positive value moves along thrust direction.
    private static final float FLAME_OFFSET = 1.0f;
    private static final float FLAME_SCALE = 1.0f;
    // Additional scale from thrust (thrust is normalized to 0..1).
    // XY controls thickness, Z controls length.
    private static final float FLAME_THRUST_XY_BOOST = 0.35f;
    private static final float FLAME_THRUST_Z_BOOST = 1.2f;
    private static final float FLAME_GLOW_SPLIT = 0.08f;

    public JetRudderRenderer(BlockEntityRendererProvider.Context context) {
    }
    @Override
    protected void renderSafe(JetRudderBlockEntity be, float partialTicks, PoseStack ms, MultiBufferSource bufferSource, int light, int overlay) {
        Vector3dc dir = be.getRenderDirection(partialTicks);
        float exhaustX = (float) -dir.x();
        float exhaustY = (float) -dir.y();
        float exhaustZ = (float) -dir.z();

        double horizontal = Math.atan2(dir.y(), dir.z());
        double vertical = Math.atan2(dir.x(), Math.sqrt(dir.y() * dir.y() + dir.z() * dir.z()));
        VertexConsumer solid = bufferSource.getBuffer(RenderType.solid());
        VertexConsumer translucent = bufferSource.getBuffer(RenderType.translucent());

        SuperByteBuffer rudder =
                CachedBufferer
                        .partial(ControlCraftPartialModels.RUDDER_PART, be.getBlockState());


        rudder
                .rotateCentered(Direction.EAST, -(float)(horizontal))
                .rotateCentered(Direction.UP, (float)(vertical + Math.PI))
                .light(light)
                .renderInto(ms, solid);

        float thrust = (float) MathUtils.clamp(be.getAnimatedThrust(partialTicks) * 1e-5, 0, 1);
        if (thrust < 0.02f) {
            return;
        }

        float scaleXY = FLAME_SCALE * (1.0f + thrust * FLAME_THRUST_XY_BOOST);
        float scaleZ = FLAME_SCALE * (0.5f + thrust * FLAME_THRUST_Z_BOOST);
//        int alpha = Math.max(48, Math.min(180, (int) (64 + thrust * 128)));
//        SuperByteBuffer flame =
//                CachedBufferer
//                        .partial(ControlCraftPartialModels.JET_RUDDER_FLAME, be.getBlockState());
//
//        flame
//                .centre()
//                .scale(scaleXY, scaleXY, scaleZ)
//                .unCentre()
//                .rotateCentered(Direction.EAST, -(float) (horizontal))
//                .rotateCentered(Direction.UP, (float) (vertical + Math.PI))
//                .translate(
//                        dir.x() * FLAME_OFFSET,
//                        dir.y() * FLAME_OFFSET,
//                        dir.z() * FLAME_OFFSET
//                )
//                .light(LightTexture.FULL_BRIGHT)
//                .color(255, 205, 125, alpha)
//                .renderInto(ms, translucent);

        // Add additive glow layers to approximate volumetric engine plume.
        VertexConsumer glow = bufferSource.getBuffer(RenderType.lightning());
        float time = AnimationTickHolder.getRenderTime(be.getLevel()) + partialTicks;
        float flickerSeed = (float) (be.getBlockPos().asLong() & 31L);
        float flicker = 0.94f + 0.06f * Mth.sin(time * 0.45f + flickerSeed);

        float coreLength = (1.05f + 1.25f * thrust) * scaleZ * flicker;
        float coreStartRadius = 0.18f * scaleXY;
        float coreEndRadius = 0.012f * scaleXY;

        float outerLength = (1.65f + 2.35f * thrust) * scaleZ * flicker;
        float outerStartRadius = 0.30f * scaleXY;
        float outerEndRadius = 0.038f * scaleXY;

        int outerR = lerpColor(thrust, 255, 48);
        int outerG = lerpColor(thrust, 64, 168);
        int outerB = lerpColor(thrust, 32, 255);

        int innerR = lerpColor(thrust, 255, 255);
        int innerG = lerpColor(thrust, 220, 255);
        int innerB = lerpColor(thrust, 80, 255);

        int outerSideR = scaleColor(outerR, 0.62f);
        int outerSideG = scaleColor(outerG, 0.62f);
        int outerSideB = scaleColor(outerB, 0.62f);

        ms.pushPose();
        ms.translate(0.5f, 0.5f, 0.5f);
        ms.mulPose(new Quaternionf().rotationTo(0f, 0f, 1f, exhaustX, exhaustY, exhaustZ));
        ms.translate(0f, 0f, FLAME_OFFSET);
        ms.mulPose(Axis.ZP.rotation(time * 0.035f));

        drawBeam(ms, glow, outerLength, outerStartRadius, outerEndRadius, outerR, outerG, outerB, 110, 0);
        drawBeam(ms, glow, coreLength, coreStartRadius, coreEndRadius, innerR, innerG, innerB, 190, 12);

        ms.pushPose();
        ms.translate(0f, FLAME_GLOW_SPLIT * scaleXY, 0f);
        drawBeam(ms, glow, outerLength * 0.92f, outerStartRadius * 0.90f, outerEndRadius, outerSideR, outerSideG, outerSideB, 72, 0);
        ms.popPose();

        ms.pushPose();
        ms.translate(0f, -FLAME_GLOW_SPLIT * scaleXY, 0f);
        drawBeam(ms, glow, outerLength * 0.92f, outerStartRadius * 0.90f, outerEndRadius, outerSideR, outerSideG, outerSideB, 72, 0);
        ms.popPose();

        ms.popPose();

    }

    @Override
    public int getViewDistance() {
        return 1024;
    }

    private static void drawBeam(
            PoseStack ms,
            VertexConsumer vc,
            float length,
            float startRadius,
            float endRadius,
            int r,
            int g,
            int b,
            int alphaStart,
            int alphaEnd
    ) {
        Matrix4f pose = ms.last().pose();

        float sx0 = -startRadius;
        float sx1 = startRadius;
        float sy0 = -startRadius;
        float sy1 = startRadius;

        float ex0 = -endRadius;
        float ex1 = endRadius;
        float ey0 = -endRadius;
        float ey1 = endRadius;

        // Side quads around +Z axis.
        addQuadGradient(vc, pose, sx0, sy0, 0f, sx1, sy0, 0f, ex1, ey0, length, ex0, ey0, length, r, g, b, alphaStart, alphaEnd);
        addQuadGradient(vc, pose, sx1, sy0, 0f, sx1, sy1, 0f, ex1, ey1, length, ex1, ey0, length, r, g, b, alphaStart, alphaEnd);
        addQuadGradient(vc, pose, sx1, sy1, 0f, sx0, sy1, 0f, ex0, ey1, length, ex1, ey1, length, r, g, b, alphaStart, alphaEnd);
        addQuadGradient(vc, pose, sx0, sy1, 0f, sx0, sy0, 0f, ex0, ey0, length, ex0, ey1, length, r, g, b, alphaStart, alphaEnd);

        // Cross quads to soften silhouette from more camera angles.
        addQuadGradient(vc, pose, 0f, sy0, 0f, 0f, sy1, 0f, 0f, ey1, length, 0f, ey0, length, r, g, b, alphaStart, alphaEnd);
        addQuadGradient(vc, pose, sx0, 0f, 0f, sx1, 0f, 0f, ex1, 0f, length, ex0, 0f, length, r, g, b, alphaStart, alphaEnd);
    }

    private static void addQuadGradient(
            VertexConsumer vc,
            Matrix4f pose,
            float x0, float y0, float z0,
            float x1, float y1, float z1,
            float x2, float y2, float z2,
            float x3, float y3, float z3,
            int r, int g, int b,
            int alphaNear,
            int alphaFar
    ) {
        vc.vertex(pose, x0, y0, z0).color(r, g, b, alphaNear).endVertex();
        vc.vertex(pose, x1, y1, z1).color(r, g, b, alphaNear).endVertex();
        vc.vertex(pose, x2, y2, z2).color(r, g, b, alphaFar).endVertex();
        vc.vertex(pose, x3, y3, z3).color(r, g, b, alphaFar).endVertex();
    }

    private static int lerpColor(float t, int start, int end) {
        return Mth.clamp(Math.round(Mth.lerp(t, start, end)), 0, 255);
    }

    private static int scaleColor(int color, float factor) {
        return Mth.clamp(Math.round(color * factor), 0, 255);
    }
}
