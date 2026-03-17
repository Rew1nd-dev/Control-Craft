package com.verr1.controlcraft.content.links.computer.lua.render;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.renderer.MultiBufferSource;

/**
 * 代表一个能在客户端 Render 线程执行的绘制指令
 */
public interface RenderCmd {

    void execute(PoseStack ms, MultiBufferSource bufferSource, int light, int overlay, float partialTick);

    default int layer() {
        return 0;
    }
}
