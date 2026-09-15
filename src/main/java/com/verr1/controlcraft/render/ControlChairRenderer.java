package com.verr1.controlcraft.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import com.mojang.math.Axis;
import com.simibubi.create.foundation.blockEntity.renderer.SafeBlockEntityRenderer;
import com.simibubi.create.foundation.render.SuperByteBuffer;
import com.simibubi.create.foundation.utility.AngleHelper;
import com.verr1.controlcraft.content.blocks.controlchair.ControlChairBlockEntity;
import com.verr1.controlcraft.registry.ControlCraftPartialModels;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.blockentity.BlockEntityRendererProvider;
import net.minecraft.core.Direction;
import net.minecraft.world.level.block.state.BlockState;

public class ControlChairRenderer extends SafeBlockEntityRenderer<ControlChairBlockEntity> {
    private static final float HANDLE_FORWARD_OFFSET = 8.0f / 16.0f;
    private static final float HANDLE_DOWN_OFFSET = 6.0f / 16.0f;

    public ControlChairRenderer(BlockEntityRendererProvider.Context context) {
    }

    @Override
    protected void renderSafe(
            ControlChairBlockEntity be,
            float partialTicks,
            PoseStack ms,
            MultiBufferSource bufferSource,
            int light,
            int overlay
    ) {
        BlockState state = be.getBlockState();
        Direction facing = be.getDirection();
        float yawAngle = be.animatedHandleRoll(partialTicks);
        float pitchAngle = be.animatedHandlePitch(partialTicks);

        VertexConsumer solid = bufferSource.getBuffer(RenderType.solid());
        SuperByteBuffer handle = CachedBufferer.partial(ControlCraftPartialModels.CHAIR_HANDLE, state);

        ms.pushPose();
        ms.translate(0.5f, 0.5f, 0.5f);
        ms.mulPose(Axis.YP.rotationDegrees(AngleHelper.horizontalAngle(facing)));
        ms.translate(0.0f, -HANDLE_DOWN_OFFSET, HANDLE_FORWARD_OFFSET);
        ms.mulPose(Axis.ZP.rotationDegrees(yawAngle));
        ms.mulPose(Axis.XP.rotationDegrees(pitchAngle));
        ms.translate(-0.5f, -0.5f, -0.5f);

        handle
                .light(light)
                .renderInto(ms, solid);

        ms.popPose();
    }

    @Override
    public int getViewDistance() {
        return 1024;
    }
}
