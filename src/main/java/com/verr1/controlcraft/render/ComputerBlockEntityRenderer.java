package com.verr1.controlcraft.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Axis;
import com.simibubi.create.foundation.blockEntity.renderer.SafeBlockEntityRenderer;
import com.verr1.controlcraft.content.links.screen_base.ComputerBaseBlockEntity;
import com.verr1.controlcraft.content.links.screen_base.lua.render.RenderCmd;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.blockentity.BlockEntityRendererProvider;
import net.minecraft.core.Direction;

import java.util.List;

public class ComputerBlockEntityRenderer extends SafeBlockEntityRenderer<ComputerBaseBlockEntity> {

    public ComputerBlockEntityRenderer(BlockEntityRendererProvider.Context context) {
        super();
    }

    @Override
    protected void renderSafe(
            ComputerBaseBlockEntity be,
            float partialTicks,
            PoseStack ms,
            MultiBufferSource bufferSource,
            int light,
            int overlay) {
        if (be.getScreen() == null)
            return;
        List<RenderCmd> cmds = be.getScreen().getActiveDisplayList();
        if (cmds == null || cmds.isEmpty())
            return;

        Direction facing = be.getDirection().getOpposite();

        ms.pushPose();

        // Standard ComputerCraft screen scaling. Assume canvas size 256x256 maps to 1
        // Block.
        float scale = 1.0f / 256.0f;

        // Origin matches OscilloscopeBlockEntityRenderer logic (raw coordinates mapping
        // to top-left of the block face)
        // Move to the center to apply initial rotation
        ms.translate(0.5, 0.5, 0.5);

        // Rotate to match the block face
        if (facing == Direction.NORTH) {
            ms.mulPose(Axis.YP.rotationDegrees(180));
        } else if (facing == Direction.SOUTH) {
            ms.mulPose(Axis.YP.rotationDegrees(0));
        } else if (facing == Direction.WEST) {
            ms.mulPose(Axis.YP.rotationDegrees(-90));
        } else if (facing == Direction.EAST) {
            ms.mulPose(Axis.YP.rotationDegrees(90));
        } else if (facing == Direction.UP) {
            ms.mulPose(Axis.XP.rotationDegrees(90));
        } else if (facing == Direction.DOWN) {
            ms.mulPose(Axis.XP.rotationDegrees(-90));
        }

        // Translate to the active face (slightly hovering above to avoid Z-fighting)
        ms.translate(0, 0, 0.501);

        // Now move coordinate origin to top-left of the face, and invert Y for 2D
        // graphics
        ms.translate(-0.5, 0.5, 0);

        // Scale coordinates logic
        ms.scale(scale, -scale, scale);

        for (RenderCmd cmd : cmds) {
            cmd.execute(ms, bufferSource, light, overlay, partialTicks);
        }

        ms.popPose();
    }

    @Override
    public int getViewDistance() {
        return 128; // Increased render distance for screens
    }
}
