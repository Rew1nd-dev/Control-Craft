package com.verr1.controlcraft.unstable.util;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.BufferBuilder;
import com.mojang.blaze3d.vertex.PoseStack;
import com.verr1.controlcraft.ControlCraft;
import net.minecraft.client.Minecraft;
import net.minecraftforge.client.event.RenderGuiEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;

import com.mojang.blaze3d.vertex.*;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraftforge.fml.common.Mod;

// @Mod.EventBusSubscriber(modid = ControlCraft.MODID, bus = Mod.EventBusSubscriber.Bus.FORGE)
public class ScreenOverlayRenderer {

    @SubscribeEvent
    public static void onRenderGui(RenderGuiEvent.Post event) {
        // 获取 PoseStack 和窗口信息
        PoseStack poseStack = event.getGuiGraphics().pose();
        int screenWidth = event.getWindow().getGuiScaledWidth();
        int screenHeight = event.getWindow().getGuiScaledHeight();

        // 开始绘制线条
        renderLine(poseStack, screenWidth, screenHeight);
    }

    private static void renderLine(PoseStack poseStack, int width, int height) {
        // 初始化渲染设置
        RenderSystem.enableBlend();
        RenderSystem.defaultBlendFunc();
        RenderSystem.setShader(GameRenderer::getPositionColorShader);

        // 定义线条的起点和终点（示例：从屏幕中心到鼠标位置）
        int startX = width / 2;
        int startY = height / 2;
        int endX = (int) Minecraft.getInstance().mouseHandler.xpos();
        int endY = (int) Minecraft.getInstance().mouseHandler.ypos();

        // 创建 Tessellator 实例
        Tesselator tessellator = Tesselator.getInstance();
        BufferBuilder buffer = tessellator.getBuilder();

        // 开始绘制线段
        buffer.begin(VertexFormat.Mode.DEBUG_LINES, DefaultVertexFormat.POSITION_COLOR);
        buffer.vertex(poseStack.last().pose(), startX, startY, 0)
                .color(1.0f, 0.0f, 0.0f, 1.0f)  // RGBA（红色）
                .endVertex();
        buffer.vertex(poseStack.last().pose(), endX, endY, 0)
                .color(1.0f, 0.0f, 0.0f, 1.0f)
                .endVertex();

        // 结束绘制
        tessellator.end();
        RenderSystem.disableBlend();
    }
}