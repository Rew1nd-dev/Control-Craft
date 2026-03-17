package com.verr1.controlcraft.content.links.computer.lua.libs;

import com.mojang.blaze3d.vertex.PoseStack;
import com.verr1.controlcraft.content.links.computer.ComputerScreen;
import com.verr1.controlcraft.content.links.computer.lua.render.DrawRectCmd;
import com.verr1.controlcraft.content.links.computer.lua.render.DrawTextCmd;
import com.verr1.controlcraft.content.links.computer.lua.render.RenderCmd;
import net.minecraft.client.renderer.MultiBufferSource;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.Varargs;
import org.luaj.vm2.lib.OneArgFunction;
import org.luaj.vm2.lib.TwoArgFunction;
import org.luaj.vm2.lib.VarArgFunction;
import org.luaj.vm2.lib.ZeroArgFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

public class RenderLib extends TwoArgFunction {

    private final List<RenderCmd> tempBuffer = new ArrayList<>();
    private final List<DrawCommandBinding> drawCommandBindings = new ArrayList<>();

    private int currentColor = 0xFFFFFFFF;
    private int currentLayer = 0;

    private final ComputerScreen screen;
    private final int canvasWidth;
    private final int canvasHeight;

    public RenderLib(ComputerScreen screen, int canvasWidth, int canvasHeight) {
        this.screen = screen;
        this.canvasWidth = Math.max(1, canvasWidth);
        this.canvasHeight = Math.max(1, canvasHeight);
        registerBuiltinDrawCommands();
    }

    private void registerBuiltinDrawCommands() {
        registerDrawCommand("drawRect", args -> {
            float x = (float) args.checkdouble(1);
            float y = (float) args.checkdouble(2);
            float w = (float) args.checkdouble(3);
            float h = (float) args.checkdouble(4);
            return new DrawRectCmd(x, y, w, h, currentColor);
        });

        registerDrawCommand("drawText", args -> {
            String text = args.checkjstring(1);
            float x = (float) args.checkdouble(2);
            float y = (float) args.checkdouble(3);
            float scale = args.narg() >= 4 ? (float) args.checkdouble(4) : 1.0f;
            return new DrawTextCmd(text, x, y, scale, currentColor);
        });
    }

    public RenderLib registerDrawCommand(String name, Function<Varargs, RenderCmd> factory) {
        drawCommandBindings.add(new DrawCommandBinding(name, factory));
        return this;
    }

    private void bindDrawCommands(LuaValue library) {
        for (DrawCommandBinding binding : drawCommandBindings) {
            library.set(binding.name(), new VarArgFunction() {
                @Override
                public Varargs invoke(Varargs args) {
                    RenderCmd cmd = binding.factory().apply(args);
                    if (cmd != null) {
                        tempBuffer.add(withCurrentLayer(cmd));
                    }
                    return NIL;
                }
            });
        }
    }

    public List<RenderCmd> collectBufferAndClear() {
        List<RenderCmd> copy = new ArrayList<>(tempBuffer);
        tempBuffer.clear();
        currentLayer = 0;
        return copy;
    }

    public void reset() {
        tempBuffer.clear();
        currentColor = 0xFFFFFFFF;
        currentLayer = 0;
    }

    @Override
    public LuaValue call(LuaValue modname, LuaValue env) {
        LuaValue library = tableOf();

        library.set("setColor", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                int r = clamp(args.checkint(1));
                int g = clamp(args.checkint(2));
                int b = clamp(args.checkint(3));
                int a = args.narg() >= 4 ? clamp(args.checkint(4)) : 255;
                currentColor = (a << 24) | (r << 16) | (g << 8) | b;
                return NIL;
            }
        });

        library.set("setAlpha", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue alpha) {
                int a = clamp(alpha.checkint());
                currentColor = (currentColor & 0x00FFFFFF) | (a << 24);
                return NIL;
            }
        });

        library.set("setOpacity", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue opacity) {
                double op = Math.max(0.0, Math.min(1.0, opacity.checkdouble()));
                int a = (int) Math.round(op * 255.0);
                currentColor = (currentColor & 0x00FFFFFF) | (a << 24);
                return NIL;
            }
        });

        // Increase current draw layer by 1 for commands emitted afterwards.
        library.set("pushLayer", new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                if (currentLayer < Integer.MAX_VALUE) {
                    currentLayer++;
                }
                return NIL;
            }
        });

        bindDrawCommands(library);

        library.set("width", LuaValue.valueOf(canvasWidth));
        library.set("height", LuaValue.valueOf(canvasHeight));
        library.set("getWidth", new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                return LuaValue.valueOf(canvasWidth);
            }
        });
        library.set("getHeight", new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                return LuaValue.valueOf(canvasHeight);
            }
        });

        library.set("clear", new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                tempBuffer.clear();
                currentLayer = 0;
                return NIL;
            }
        });

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

    private RenderCmd withCurrentLayer(RenderCmd cmd) {
        if (currentLayer <= 0) {
            return cmd;
        }
        return new LayeredRenderCmd(currentLayer, cmd);
    }

    private record DrawCommandBinding(String name, Function<Varargs, RenderCmd> factory) {
    }

    private record LayeredRenderCmd(int layer, RenderCmd delegate) implements RenderCmd {
        @Override
        public void execute(PoseStack ms, MultiBufferSource bufferSource, int light, int overlay, float partialTick) {
            delegate.execute(ms, bufferSource, light, overlay, partialTick);
        }
    }
}
