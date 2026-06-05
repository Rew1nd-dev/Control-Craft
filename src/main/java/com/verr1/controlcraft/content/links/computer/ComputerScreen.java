package com.verr1.controlcraft.content.links.computer;

import com.verr1.controlcraft.content.links.computer.lua.ComputerDelegateHandler;
import com.verr1.controlcraft.content.links.computer.lua.render.RenderCmd;

import java.util.ArrayList;
import java.util.List;

public class ComputerScreen extends ComputerDelegateHandler {

    private static final float MAX_OFFSET = 10.0f;

    private volatile List<RenderCmd> activeDisplayList = new ArrayList<>();
    private volatile ComputerDisplayMetrics displayMetrics = ComputerDisplayMetrics.DEFAULT;
    private volatile float offsetX = 0.0f;
    private volatile float offsetY = 0.0f;
    private volatile float offsetZ = 0.0f;

    public ComputerScreen(ComputerBlockEntity delegate) {
        super(delegate);
    }

    public void updateCommands(List<RenderCmd> newCmds) {
        this.activeDisplayList = newCmds;
    }

    public List<RenderCmd> getActiveDisplayList() {
        return activeDisplayList;
    }

    public ComputerDisplayMetrics getDisplayMetrics() {
        return displayMetrics;
    }

    public void setDisplayMetrics(ComputerDisplayMetrics displayMetrics) {
        this.displayMetrics = displayMetrics == null ? ComputerDisplayMetrics.DEFAULT : displayMetrics;
    }

    public void setRenderOffset(float x, float y, float z) {
        this.offsetX = clampOffset(x);
        this.offsetY = clampOffset(y);
        this.offsetZ = clampOffset(z);
    }

    public float getOffsetX() {
        return offsetX;
    }

    public float getOffsetY() {
        return offsetY;
    }

    public float getOffsetZ() {
        return offsetZ;
    }

    private float clampOffset(float v) {
        return Math.max(-MAX_OFFSET, Math.min(MAX_OFFSET, v));
    }
}