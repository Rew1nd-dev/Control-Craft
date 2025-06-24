package com.verr1.controlcraft.content.gui.widgets;

import com.simibubi.create.foundation.gui.widget.AbstractSimiWidget;
import net.minecraft.ChatFormatting;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import org.jetbrains.annotations.NotNull;

import java.awt.*;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class MultiPlotWidget extends AbstractSimiWidget {
    public static final Color[] colors = {
            new Color(0xFF0000).darker().darker(), // Red
            new Color(0x00FF00).darker().darker(), // Green
            new Color(0x0000FF).darker().darker(), // Blue
            new Color(0xFFFF00).darker().darker(), // Yellow
            new Color(0xFF00FF).darker().darker(), // Magenta
            new Color(0x00FFFF).darker().darker(), // Cyan
            new Color(0xFFFFFF).darker().darker()  // White
    };

    protected final ChannelDataSupplier fetcher;
    protected final Font font;

    protected double minValue = 0.0;
    protected double maxValue = 1.0;

    protected double dataMax = 1.0;
    protected double dataMin = 1.0;

    public MultiPlotWidget(
            Font font,
            int x,
            int y,
            int width,
            int height,
            ChannelDataSupplier fetcher

    ) {
        super(x, y, width, height);
        this.fetcher = fetcher;
        this.font = font;
    }

    @Override
    protected void doRender(@NotNull GuiGraphics graphics, int mouseX, int mouseY, float partialTicks) {
        int channelSize = fetcher.size();
        for (int i = 0; i < channelSize; i++){
            renderCurve(graphics, fetcher.safeFetch(i), i);
        }
    }

    @Override
    protected void onDrag(double p_93636_, double p_93637_, double p_93638_, double p_93639_) {
        super.onDrag(p_93636_, p_93637_, p_93638_, p_93639_);
    }

    public void fit(){
        // 获取所有通道的数据
        double len = Math.max((dataMax - dataMin), 0.1);

        maxValue = (dataMax + dataMin) * 0.5 + 0.8 * len;
        minValue = (dataMax + dataMin) * 0.5 - 0.8 * len;
    }

    public void autoFit(){
        if (dataMax > maxValue || dataMin < minValue)fit();
    }

    protected void cacheRange(){
        List<Stream<Double>> allData = new ArrayList<>();
        for (int i = 0; i < fetcher.size(); i++) {
            allData.add(fetcher.safeFetch(i));
        }

        // 合并所有数据流
        Stream<Double> combinedStream = allData.stream().flatMap(s -> s);

        // 计算最小值和最大值
        List<Double> values = combinedStream.toList();
        if (values.isEmpty()) {
            return; // 如果没有数据，直接返回
        }

        dataMin = values.stream().min(Double::compareTo).orElse(0.0);
        dataMax = values.stream().max(Double::compareTo).orElse(1.0);
    }

    protected void renderAxes(@NotNull GuiGraphics graphics){
        // 绘制 x 轴和 y 轴
        int widgetWidth = this.width;
        int widgetHeight = this.height;

        // 绘制 x 轴
        graphics.fill(this.getX(), this.getY() + widgetHeight - 1, this.getX() + widgetWidth, this.getY() + widgetHeight, 0xFF000000);
        // 绘制 y 轴
        graphics.fill(this.getX(), this.getY(), this.getX() + 1, this.getY() + widgetHeight, 0xFF000000);

        // 绘制轴标签
        Component Time = Component.literal("Time").withStyle(s -> s.withColor(ChatFormatting.DARK_AQUA));
        Component Value = Component.literal("Value").withStyle(s -> s.withColor(ChatFormatting.DARK_AQUA));
        graphics.drawString(font, Time, this.getX() + widgetWidth, this.getY() + widgetHeight - 12, 3);
        graphics.drawString(font, Value, this.getX() + 2, this.getY() - font.lineHeight / 2, 3);
        int slices = 4;
        for (int i = 0; i <= slices; i++){
            double yValue = minValue + (maxValue - minValue) * i / slices;
            int yPos = (int) ((1.0 - (yValue - minValue) / (maxValue - minValue)) * widgetHeight);
            Component label = Component.literal(String.format("%.2f", yValue)).withStyle(s -> s.withColor(ChatFormatting.DARK_AQUA));
            graphics.drawString(font, label,
                    this.getX() - font.width(label) - 2,
                    this.getY() + yPos -  font.lineHeight / 2,
                    3
            );
            // draw small line
            graphics.fill(this.getX() - 2, this.getY() + yPos - 1, this.getX(), this.getY() + yPos, 0xFF000000);
        }

        for (int i = 1; i <= slices; i++){
            double xValue = (double) i / slices * fetcher.span();

            Component label = Component.literal(String.format("%.2f", xValue)).withStyle(s -> s.withColor(ChatFormatting.DARK_AQUA));
            int xPos = (int) (xValue * widgetWidth);
            graphics.drawString(font, label,
                    this.getX() + xPos - font.width(label) / 2,
                    this.getY() + widgetHeight,
                    3
            );
            // draw small line
            graphics.fill(this.getX() + xPos - 1, this.getY() + widgetHeight - 2, this.getX() + xPos, this.getY() + widgetHeight, 0xFF000000);
        }

    }

    protected void renderCurve(@NotNull GuiGraphics graphics, Stream<Double> data, int index) {
        // 将 Stream 转换为 List 以便处理
        List<Double> dataList = data.toList();
        int dataSize = dataList.size();

        // 如果数据点少于 1，无法绘制散点，直接返回
        if (dataSize < 1) {
            return;
        }
        cacheRange();
        autoFit();
        renderAxes(graphics);
        // 确定数据的最小值和最大值

        double valueRange = maxValue - minValue;

        // 避免除以零的情况
        if (valueRange <= 0) {
            valueRange = 1.0;
        }

        // 获取 widget 的尺寸
        int widgetWidth = this.width;
        int widgetHeight = this.height;

        // 计算 x 轴上每个数据点的间距
        double xStep = (double) widgetWidth / (dataSize - 1);

        // 将数据转换为屏幕坐标点
        List<Point> points = new ArrayList<>();
        for (int i = 0; i < dataSize; i++) {
            double value = dataList.get(i);
            // 将数据值归一化并映射到 widget 高度
            double normalizedValue = (value - minValue) / valueRange;
            // Minecraft 的 y 轴向下增长，因此从底部计算 y 坐标
            int y = (int) (widgetHeight - normalizedValue * widgetHeight);
            int x = (int) (i * xStep);
            points.add(new Point(x, y));
        }

        // 绘制散点：为每个点绘制一个小方块
        for (Point p : points) {
            graphics.fill(p.x + this.getX(), p.y + this.getY(), p.x + this.getX() + 2, p.y + this.getY() + 2, colors[index % colors.length].getRGB());
        }
    }

    public interface ChannelDataSupplier {
        int size();

        double span();

        Stream<Double> fetch(int channel);

        default Stream<Double> safeFetch(int channel){
            if (channel < 0 || channel >= size()) {
                return Stream.empty();
            }
            return fetch(channel);
        }
    }
}
