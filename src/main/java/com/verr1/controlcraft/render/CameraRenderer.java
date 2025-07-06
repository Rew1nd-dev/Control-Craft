package com.verr1.controlcraft.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import com.simibubi.create.foundation.blockEntity.renderer.SafeBlockEntityRenderer;
import com.simibubi.create.foundation.outliner.LineOutline;
import com.simibubi.create.foundation.outliner.Outline;
import com.simibubi.create.foundation.render.SuperByteBuffer;
import com.simibubi.create.foundation.render.SuperRenderTypeBuffer;
import com.simibubi.create.foundation.utility.Color;
import com.verr1.controlcraft.content.blocks.camera.CameraBlockEntity;
import com.verr1.controlcraft.foundation.data.render.Line;
import com.verr1.controlcraft.foundation.data.render.RayLerpHelper;
import com.verr1.controlcraft.foundation.managers.ClientCameraManager;
import com.verr1.controlcraft.registry.ControlCraftPartialModels;
import kotlin.Pair;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.blockentity.BlockEntityRendererProvider;
import net.minecraft.core.Direction;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.Vec3;
import org.joml.Vector3dc;

public class CameraRenderer extends SafeBlockEntityRenderer<CameraBlockEntity> {
    public CameraRenderer(BlockEntityRendererProvider.Context context) {

    }

    @Override
    protected void renderSafe(
            CameraBlockEntity be,
            float partialTicks,
            PoseStack ms,
            MultiBufferSource bufferSource,
            int light,
            int overlay
    ) {
        CameraBlockEntity linkedCamera = ClientCameraManager.getLinkedCamera();
        if(linkedCamera != null && linkedCamera.getBlockPos().equals(be.getBlockPos()))return;

        Vector3dc view = be.getLocViewForward();


        BlockState state = be.getBlockState();
        VertexConsumer solid = bufferSource.getBuffer(RenderType.translucent());
        SuperByteBuffer lensBuffer = CachedBufferer.partialFacing(ControlCraftPartialModels.CAMERA_LENS, state);
        SuperByteBuffer yawBuffer = CachedBufferer.partialFacing(ControlCraftPartialModels.CAMERA_YAW, state);

        Direction horizontal = be.getDirection();

        Pair<Double, Double> hv = angle(horizontal, view);

        Direction vertical = switch (horizontal){
            case WEST -> Direction.NORTH;
            case EAST -> Direction.SOUTH;
            case UP, SOUTH -> Direction.EAST;
            case DOWN, NORTH -> Direction.WEST;
        };



        yawBuffer
                .rotateCentered(horizontal, hv.getFirst().floatValue())
                .light(light)
                .renderInto(ms, solid);

        lensBuffer
                .rotateCentered(horizontal, hv.getFirst().floatValue())
                .rotateCentered(vertical, hv.getSecond().floatValue())
                .light(light)
                .renderInto(ms, solid);
    }


    private static Pair<Double, Double> angle(Direction face, Vector3dc view){
        double horizontal = switch (face){
            case NORTH -> Math.atan2(-view.x(), -view.y()); // -z
            case SOUTH -> -Math.atan2(-view.x(), -view.y());// +z
            case EAST -> Math.atan2(-view.z(), -view.y());
            case WEST -> -Math.atan2(-view.z(), -view.y());
            case UP -> Math.atan2(-view.x(), -view.z());
            case DOWN -> -Math.atan2(-view.x(), -view.z());
        };

        double vertical = switch (face){
            case NORTH -> Math.asin(view.z());
            case SOUTH -> Math.asin(-view.z());
            case EAST -> Math.asin(view.x());
            case WEST -> Math.asin(-view.x());
            case UP -> Math.asin(view.y()) + Math.PI;
            case DOWN -> Math.asin(-view.y());
        };

        return new Pair<>(horizontal, vertical);

    }

}
