package com.verr1.controlcraft.content.links.screen_base.lua.render;

import com.verr1.controlcraft.content.links.screen_base.ComputerScreen;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.Varargs;
import org.luaj.vm2.lib.TwoArgFunction;
import org.luaj.vm2.lib.VarArgFunction;
import org.luaj.vm2.lib.ZeroArgFunction;

import java.util.ArrayList;
import java.util.List;

public class RenderLib extends TwoArgFunction {

    // 每一帧 Lua 构建的临时指令列表
    private final List<RenderCmd> tempBuffer = new ArrayList<>();

    // 当前画笔的颜色 (ARGB 格式)，默认为不透明白色
    private int currentColor = 0xFFFFFFFF;

    // 渲染宿主
    private final ComputerScreen screen;

    public RenderLib(ComputerScreen screen) {
        this.screen = screen;
    }

    /**
     * @return 获取当前已经构建好的渲染指令列表的拷贝（用于传给 Java 渲染线程）
     */
    public List<RenderCmd> collectBufferAndClear() {
        List<RenderCmd> copy = new ArrayList<>(tempBuffer);
        tempBuffer.clear();
        return copy;
    }

    // 强制清空
    public void reset() {
        tempBuffer.clear();
        currentColor = 0xFFFFFFFF;
    }

    @Override
    public LuaValue call(LuaValue modname, LuaValue env) {
        LuaValue library = tableOf();

        // render.setColor(r, g, b, [a])
        library.set("setColor", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                int r = clamp(args.checkint(1));
                int g = clamp(args.checkint(2));
                int b = clamp(args.checkint(3));
                int a = args.narg() >= 4 ? clamp(args.checkint(4)) : 255;

                // 转换为 Minecraft 原生的 ARGB 整数
                currentColor = (a << 24) | (r << 16) | (g << 8) | b;
                return NIL;
            }
        });

        // render.drawRect(x, y, w, h)
        library.set("drawRect", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                float x = (float) args.checkdouble(1);
                float y = (float) args.checkdouble(2);
                float w = (float) args.checkdouble(3);
                float h = (float) args.checkdouble(4);

                tempBuffer.add(new DrawRectCmd(x, y, w, h, currentColor));
                return NIL;
            }
        });

        // render.drawText(text, x, y, [scale])
        library.set("drawText", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                String text = args.checkjstring(1);
                float x = (float) args.checkdouble(2);
                float y = (float) args.checkdouble(3);
                float scale = args.narg() >= 4 ? (float) args.checkdouble(4) : 1.0f;

                tempBuffer.add(new DrawTextCmd(text, x, y, scale, currentColor));
                return NIL;
            }
        });

        // render.clear()
        library.set("clear", new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                tempBuffer.clear();
                return NIL;
            }
        });

        // render.submit()
        library.set("submit", new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                if (screen != null) {
                    screen.updateCommands(collectBufferAndClear());
                }
                return NIL;
            }
        });

        env.set("render", library);
        return library;
    }

    private int clamp(int val) {
        return Math.max(0, Math.min(255, val));
    }
}
