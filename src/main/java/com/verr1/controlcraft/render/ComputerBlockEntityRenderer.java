package com.verr1.controlcraft.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Axis;
import com.simibubi.create.foundation.blockEntity.renderer.SafeBlockEntityRenderer;
import com.verr1.controlcraft.content.links.computer.ComputerBlockEntity;
import com.verr1.controlcraft.content.links.computer.ComputerDisplayMetrics;
import com.verr1.controlcraft.content.links.computer.lua.render.RenderCmd;
import net.minecraft.client.renderer.LightTexture;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.blockentity.BlockEntityRendererProvider;
import net.minecraft.core.Direction;

import java.util.List;

public class ComputerBlockEntityRenderer extends SafeBlockEntityRenderer<ComputerBlockEntity> {

    public ComputerBlockEntityRenderer(BlockEntityRendererProvider.Context context) {
        super();
    }

    @Override
    protected void renderSafe(
            ComputerBlockEntity be,
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
        ComputerDisplayMetrics metrics = be.displayMetrics();

        ms.pushPose();

        // Move to block center first.
        ms.translate(0.5, 0.5, 0.5);

        // Hologram anchor: from the original face, move +1 block in world up and
        // 0.5 block toward the back of the screen facing direction.
        ms.translate(0.0, 1.0, 0.0);
        ms.translate(-facing.getStepX() * 0.5, -facing.getStepY() * 0.5, -facing.getStepZ() * 0.5);

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

        // Move origin to top-left of the virtual surface, then map pixel coordinates
        // into the configured world-space surface size.
        ms.translate(-metrics.surfaceWidth() * 0.5f, metrics.surfaceHeight() * 0.5f, 0);
        ms.scale(
                metrics.surfaceWidth() / metrics.pixelWidth(),
                -metrics.surfaceHeight() / metrics.pixelHeight(),
                1.0f
        );

        final float layerStep = 0.0005f;
        final float orderStep = 0.00001f;
        int drawOrder = 0;
        for (RenderCmd cmd : cmds) {
            ms.pushPose();
            float zOffset = cmd.layer() * layerStep + drawOrder * orderStep;
            ms.translate(0, 0, zOffset);
            cmd.execute(ms, bufferSource, LightTexture.FULL_BRIGHT, overlay, partialTicks);
            ms.popPose();
            drawOrder++;
        }

        ms.popPose();
    }

    @Override
    public int getViewDistance() {
        return 128; // Increased render distance for screens
    }
}
