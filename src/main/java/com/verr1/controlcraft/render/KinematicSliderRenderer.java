package com.verr1.controlcraft.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import com.simibubi.create.foundation.blockEntity.renderer.SafeBlockEntityRenderer;
import com.simibubi.create.foundation.render.SuperByteBuffer;
import com.verr1.controlcraft.content.blocks.slider.DynamicSliderBlockEntity;
import com.verr1.controlcraft.content.blocks.slider.KinematicSliderBlockEntity;
import com.verr1.controlcraft.registry.ControlCraftPartialModels;
import com.verr1.controlcraft.utils.VSMathUtils;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.blockentity.BlockEntityRendererProvider;
import net.minecraft.world.level.block.state.BlockState;
import org.joml.Vector3f;
import org.joml.Vector3fc;

public class KinematicSliderRenderer extends SafeBlockEntityRenderer<KinematicSliderBlockEntity> {
    public KinematicSliderRenderer(BlockEntityRendererProvider.Context context) {
    }

    @Override
    protected void renderSafe(KinematicSliderBlockEntity be, float partialTicks, PoseStack ms, MultiBufferSource
    bufferSource, int light, int overlay) {
        float distance = (float) VSMathUtils.clamp(be.getAnimatedTargetDistance(partialTicks), 32) ;
        BlockState state = be.getBlockState();
        VertexConsumer solid = bufferSource.getBuffer(RenderType.solid());
        SuperByteBuffer buffer_top = CachedBufferer.partialFacing(ControlCraftPartialModels.CONSTRAINT_SLIDER_TOP, state);


        buffer_top.translate(new Vector3f(be.getDirectionJOML().get(new Vector3f()).mul(distance)))
                .light(light)
                .renderInto(ms, solid);
    }
}
