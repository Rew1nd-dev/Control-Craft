package com.verr1.controlcraft.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Axis;
import com.simibubi.create.foundation.blockEntity.renderer.SafeBlockEntityRenderer;
import com.verr1.controlcraft.content.links.scope.OscilloscopeBlockEntity;
import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.renderer.LightTexture;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.blockentity.BlockEntityRendererProvider;
import net.minecraft.core.Direction;
import net.minecraft.network.chat.Component;

public class OscilloscopeBlockEntityRenderer extends CimulinkSocketRenderer<OscilloscopeBlockEntity> {

    public OscilloscopeBlockEntityRenderer(BlockEntityRendererProvider.Context context) {
        super(context);
    }

    @Override
    protected void renderSafe(
            OscilloscopeBlockEntity be,
            float partialTicks,
            PoseStack ms,
            MultiBufferSource bufferSource,
            int light,
            int overlay
    ) {
        super.renderSafe(be, partialTicks, ms, bufferSource, light, overlay);
        double value = be.peekMain();
        Direction facing = be.getDirection().getOpposite();

        // render value as string:
        Font font = Minecraft.getInstance().font;;
        String valueString = String.format("%.2f", value);
        ms.pushPose();
        int digits = Math.max(4, valueString.length());
        // transform to face:
        int basicDigits = 4;
        float scale = 0.015f * basicDigits / digits;
        ms.scale(scale, -scale, scale); // Scale to fit the block
        rotateToFace(facing, ms);
        translateToFace(facing, ms, scale); // Adjust position as needed

        font.drawInBatch(
                Component.literal(valueString).withStyle(s -> s.withColor(ChatFormatting.DARK_AQUA)),
                -font.width(valueString) / 2f,
                0.0f,
                0,
                false,
                ms.last().pose(),
                bufferSource,
                Font.DisplayMode.NORMAL,
                0,
                LightTexture.FULL_BRIGHT
        );
        ms.popPose();

    }

    public static void rotateToFace(Direction dir, PoseStack ms){
        switch (dir) {
            case NORTH -> ms.mulPose(Axis.YP.rotationDegrees(180));
            case SOUTH -> ms.mulPose(Axis.XP.rotationDegrees(0));
            case WEST -> ms.mulPose(Axis.YP.rotationDegrees(-90));
            case EAST -> ms.mulPose(Axis.YP.rotationDegrees(90));
            case UP -> ms.mulPose(Axis.XP.rotationDegrees(90));
            case DOWN -> ms.mulPose(Axis.XP.rotationDegrees(-90));
        }
    }

    public static void translateToFace(Direction dir, PoseStack ms, double scale){
        double distance = 1;
        double offset = -0.15 / scale; // Offset to center the text
        double x_offset = 0 / scale;
        double y_offset = -0.15 / scale;
        switch (dir) {
            case NORTH ->      ms.translate(-0.5 / scale + x_offset, -0.5 / scale + y_offset, 0 + offset ); // 北面 (z = -0.5)
            case SOUTH ->      ms.translate( 0.5 / scale + x_offset, -0.5 / scale + y_offset, distance / scale + offset);
            case WEST, DOWN -> ms.translate( 0.5 / scale + x_offset, -0.5 / scale + y_offset, 0 + offset);
            case EAST  ->      ms.translate(-0.5 / scale + x_offset, -0.5 / scale + y_offset, distance / scale + offset);
            case UP    ->      ms.translate( 0.5 / scale + x_offset,  0.5 / scale + y_offset, distance / scale + offset);
        }
    }

}
