package com.verr1.controlcraft.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import com.simibubi.create.foundation.blockEntity.renderer.SafeBlockEntityRenderer;
import com.simibubi.create.foundation.render.SuperByteBuffer;
import com.verr1.controlcraft.content.blocks.camera.CameraBlockEntity;
import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.content.links.CimulinkRenderer;
import com.verr1.controlcraft.registry.ControlCraftPartialModels;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.blockentity.BlockEntityRendererProvider;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.Vec3;

import java.util.List;

public class CimulinkSocketRenderer<T extends CimulinkBlockEntity<?>> extends SafeBlockEntityRenderer<T> {


    public CimulinkSocketRenderer(BlockEntityRendererProvider.Context context) {
    }



    @Override
    protected void renderSafe(
            T be,
            float partialTicks,
            PoseStack ms,
            MultiBufferSource bufferSource,
            int light,
            int overlay
    ) {
        BlockState state = be.getBlockState();
        List<Vec3> sockets = List.copyOf(((CimulinkRenderer)be.renderer()).socketPositions());
        SuperByteBuffer socketBuffer = CachedBufferer.partialFacing(ControlCraftPartialModels.SOCKET, state);
        VertexConsumer solid = bufferSource.getBuffer(RenderType.solid());

        for (Vec3 socket : sockets) {
            socketBuffer
                    .translate(socket.x, socket.y, socket.z)
                    .light(light)
                    .renderInto(ms, solid);
        }

    }
}
