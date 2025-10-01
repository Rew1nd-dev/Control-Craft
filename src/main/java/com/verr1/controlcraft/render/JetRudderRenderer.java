package com.verr1.controlcraft.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import com.simibubi.create.foundation.blockEntity.renderer.SafeBlockEntityRenderer;
import com.simibubi.create.foundation.render.SuperByteBuffer;
import com.simibubi.create.foundation.utility.Couple;
import com.verr1.controlcraft.content.blocks.jet.JetRudderBlockEntity;
import com.verr1.controlcraft.registry.ControlCraftPartialModels;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.blockentity.BlockEntityRendererProvider;
import net.minecraft.core.Direction;
import org.joml.Vector2dc;
import org.joml.Vector3d;
import org.joml.Vector3dc;

public class JetRudderRenderer extends SafeBlockEntityRenderer<JetRudderBlockEntity> {
    public JetRudderRenderer(BlockEntityRendererProvider.Context context) {
    }
    @Override
    protected void renderSafe(JetRudderBlockEntity be, float partialTicks, PoseStack ms, MultiBufferSource bufferSource, int light, int overlay) {
        float base_offset = (float) Math.toRadians(0);

        // Couple<Double> angles = be.getRenderAngles();

        Vector3dc dir = be.getRenderDirection(partialTicks);

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

    }

    @Override
    public int getViewDistance() {
        return 1024;
    }
}
