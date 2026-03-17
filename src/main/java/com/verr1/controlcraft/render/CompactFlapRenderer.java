package com.verr1.controlcraft.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import com.simibubi.create.foundation.blockEntity.renderer.SafeBlockEntityRenderer;
import com.simibubi.create.foundation.render.SuperByteBuffer;
import com.verr1.controlcraft.content.blocks.flap.CompactFlapBlockEntity;
import com.verr1.controlcraft.content.blocks.flap.FlapBearingBlockEntity;
import com.verr1.controlcraft.registry.ControlCraftPartialModels;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.blockentity.BlockEntityRendererProvider;
import net.minecraft.core.Direction;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.BlockStateProperties;
import net.minecraft.world.phys.Vec3;

public class CompactFlapRenderer extends SafeBlockEntityRenderer<CompactFlapBlockEntity> {
    public CompactFlapRenderer(BlockEntityRendererProvider.Context context) {
    }

    @Override
    protected void renderSafe(CompactFlapBlockEntity be, float partialTicks, PoseStack ms, MultiBufferSource bufferSource, int light, int overlay) {
        float angle = be.getClientAnimatedAngle().getValue(partialTicks);// + (float)be.getOffset();
        float tilt = be.getClientAnimatedTilt().getValue(partialTicks);
        Direction dir = be.getDirection();

        Direction off = be.clientRenderVertical();
        double off_o = be.clientRenderOffset();
        Vec3 off_v = new Vec3(off.getStepX(), off.getStepY(), off.getStepZ()).scale(off_o);

        // int sign = (dir == Direction.UP || dir == Direction.SOUTH || dir == Direction.EAST) ? 1 : -1;
        BlockState state = be.getBlockState();

        VertexConsumer solid = bufferSource.getBuffer(RenderType.solid());
        SuperByteBuffer flapBuffer = CachedBufferer.partialFacing(ControlCraftPartialModels.WING_CONTROLLER_TOP, state);

        flapBuffer
                .translate(off_v)
                .rotateCentered(be.leftDirection(), (float) Math.toRadians(tilt))
                .rotateCentered(dir, (float) Math.toRadians(angle))
                .light(light)
                .renderInto(ms, solid);
    }

    @Override
    public int getViewDistance() {
        return 1024;
    }
}
