package com.verr1.controlcraft.content.links.computer.lua.libs;

import com.mojang.blaze3d.vertex.PoseStack;
import com.verr1.controlcraft.content.links.computer.ComputerDisplayMetrics;
import com.verr1.controlcraft.content.links.computer.ComputerScreen;
import com.verr1.controlcraft.content.links.computer.lua.render.DrawCircleCmd;
import com.verr1.controlcraft.content.links.computer.lua.render.DrawCircleOutlineCmd;
import com.verr1.controlcraft.content.links.computer.lua.render.DrawLineCmd;
import com.verr1.controlcraft.content.links.computer.lua.render.DrawRectCmd;
import com.verr1.controlcraft.content.links.computer.lua.render.DrawRectOutlineCmd;
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

    private static final int MIN_RESOLUTION = 1;
    private static final int MAX_RESOLUTION = 4096;
    private static final float MIN_SURFACE_SIZE = 0.05f;
    private static final float MAX_SURFACE_SIZE = 64.0f;
    private static final float DEFAULT_LINE_THICKNESS = 1.0f;
    private static final int DEFAULT_CIRCLE_SEGMENTS = 32;

    private final List<RenderCmd> tempBuffer = new ArrayList<>();
    private final List<DrawCommandBinding> drawCommandBindings = new ArrayList<>();

    private int currentColor = 0xFFFFFFFF;
    private int currentLayer = 0;

    private final ComputerScreen screen;
    private int canvasWidth;
    private int canvasHeight;
    private float surfaceWidth;
    private float surfaceHeight;

    public RenderLib(ComputerScreen screen) {
        this.screen = screen;

        ComputerDisplayMetrics metrics = screen == null ? ComputerDisplayMetrics.DEFAULT : screen.getDisplayMetrics();
        this.canvasWidth = clampResolution(metrics.pixelWidth());
        this.canvasHeight = clampResolution(metrics.pixelHeight());
        this.surfaceWidth = clampSurface((float) metrics.surfaceWidth());
        this.surfaceHeight = clampSurface((float) metrics.surfaceHeight());

        applyMetricsToScreen();
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

        registerDrawCommand("drawLine", args -> {
            float x0 = (float) args.checkdouble(1);
            float y0 = (float) args.checkdouble(2);
            float x1 = (float) args.checkdouble(3);
            float y1 = (float) args.checkdouble(4);
            float thickness = args.narg() >= 5 ? (float) args.checkdouble(5) : DEFAULT_LINE_THICKNESS;
            return new DrawLineCmd(x0, y0, x1, y1, thickness, currentColor);
        });

        registerDrawCommand("drawRectOutline", args -> {
            float x = (float) args.checkdouble(1);
            float y = (float) args.checkdouble(2);
            float w = (float) args.checkdouble(3);
            float h = (float) args.checkdouble(4);
            float thickness = args.narg() >= 5 ? (float) args.checkdouble(5) : DEFAULT_LINE_THICKNESS;
            return new DrawRectOutlineCmd(x, y, w, h, thickness, currentColor);
        });

        registerDrawCommand("drawCircle", args -> {
            float cx = (float) args.checkdouble(1);
            float cy = (float) args.checkdouble(2);
            float radius = (float) args.checkdouble(3);
            int segments = args.narg() >= 4 ? args.checkint(4) : DEFAULT_CIRCLE_SEGMENTS;
            return new DrawCircleCmd(cx, cy, radius, segments, currentColor);
        });

        registerDrawCommand("drawCircleOutline", args -> {
            float cx = (float) args.checkdouble(1);
            float cy = (float) args.checkdouble(2);
            float radius = (float) args.checkdouble(3);
            float thickness = args.narg() >= 4 ? (float) args.checkdouble(4) : DEFAULT_LINE_THICKNESS;
            int segments = args.narg() >= 5 ? args.checkint(5) : DEFAULT_CIRCLE_SEGMENTS;
            return new DrawCircleOutlineCmd(cx, cy, radius, thickness, segments, currentColor);
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
                int r = clampColor(args.checkint(1));
                int g = clampColor(args.checkint(2));
                int b = clampColor(args.checkint(3));
                int a = args.narg() >= 4 ? clampColor(args.checkint(4)) : 255;
                currentColor = (a << 24) | (r << 16) | (g << 8) | b;
                return NIL;
            }
        });

        library.set("setAlpha", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue alpha) {
                int a = clampColor(alpha.checkint());
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

        library.set("setResolution", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue w, LuaValue h) {
                canvasWidth = clampResolution(w.checkint());
                canvasHeight = clampResolution(h.checkint());
                applyMetricsToScreen();
                updateSizeExports(library);
                return NIL;
            }
        });

        library.set("setSurfaceSize", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue w, LuaValue h) {
                surfaceWidth = clampSurface((float) w.checkdouble());
                surfaceHeight = clampSurface((float) h.checkdouble());
                applyMetricsToScreen();
                return NIL;
            }
        });

        library.set("setOffset", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                float x = args.narg() >= 1 ? (float) args.checkdouble(1) : 0.0f;
                float y = args.narg() >= 2 ? (float) args.checkdouble(2) : 0.0f;
                float z = args.narg() >= 3 ? (float) args.checkdouble(3) : 0.0f;
                if (screen != null) {
                    screen.setRenderOffset(x, y, z);
                }
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
        updateSizeExports(library);

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
        library.set("getSurfaceWidth", new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                return LuaValue.valueOf(surfaceWidth);
            }
        });
        library.set("getSurfaceHeight", new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                return LuaValue.valueOf(surfaceHeight);
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

    private void updateSizeExports(LuaValue library) {
        library.set("width", LuaValue.valueOf(canvasWidth));
        library.set("height", LuaValue.valueOf(canvasHeight));
    }

    private void applyMetricsToScreen() {
        if (screen == null) {
            return;
        }
        screen.setDisplayMetrics(new ComputerDisplayMetrics(canvasWidth, canvasHeight, surfaceWidth, surfaceHeight));
    }

    private int clampColor(int val) {
        return Math.max(0, Math.min(255, val));
    }

    private int clampResolution(int val) {
        return Math.max(MIN_RESOLUTION, Math.min(MAX_RESOLUTION, val));
    }

    private float clampSurface(float val) {
        return Math.max(MIN_SURFACE_SIZE, Math.min(MAX_SURFACE_SIZE, val));
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
