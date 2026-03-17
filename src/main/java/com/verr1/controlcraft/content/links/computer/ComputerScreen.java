package com.verr1.controlcraft.content.links.computer;

import com.verr1.controlcraft.content.links.computer.lua.ComputerDelegateHandler;
import com.verr1.controlcraft.content.links.computer.lua.render.RenderCmd;

import java.util.ArrayList;
import java.util.List;

public class ComputerScreen extends ComputerDelegateHandler {

    // 存储当前需要绘制的命令集合，这里使用 volatile 或其他同步机制保证线程安全，
    // 因为 Lua 线程 (Client Tick) 和渲染线程 (Render) 会并发访问。
    private volatile List<RenderCmd> activeDisplayList = new ArrayList<>(); //List.of(new DrawRectCmd(64, 64, 16, 16, Color.RED.getRGB()))

    public ComputerScreen(ComputerBlockEntity delegate) {
        super(delegate);
    }

    public void updateCommands(List<RenderCmd> newCmds) {
        this.activeDisplayList = newCmds;
    }

    public List<RenderCmd> getActiveDisplayList() {
        return activeDisplayList;
    }

}
