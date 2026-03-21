package com.verr1.controlcraft.content.links.computer.lua.libs;

import com.verr1.controlcraft.content.links.computer.lua.IComputerCommonContext;
import org.joml.Vector3dc;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.Varargs;
import org.luaj.vm2.lib.TwoArgFunction;
import org.luaj.vm2.lib.ZeroArgFunction;

public class BlockLib extends TwoArgFunction {

    private final IComputerCommonContext context;

    public BlockLib(IComputerCommonContext context) {
        this.context = context;
    }

    @Override
    public LuaValue call(LuaValue modname, LuaValue env) {
        LuaValue library = tableOf();

        library.set("frontLocal", vectorGetter(env, context::frontLocal));
        library.set("front", vectorGetter(env, context::front));
        library.set("leftLocal", vectorGetter(env, context::leftLocal));
        library.set("left", vectorGetter(env, context::left));
        library.set("upLocal", vectorGetter(env, context::upLocal));
        library.set("up", vectorGetter(env, context::up));
        library.set("yardPosition", vectorGetter(env, context::positionModel));

        env.set("Block", library);
        return library;
    }

    private ZeroArgFunction vectorGetter(LuaValue env, VectorSupplier supplier) {
        return new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                return toLuaVector(env, supplier.get());
            }
        };
    }

    private LuaValue toLuaVector(LuaValue env, Vector3dc vector) {
        LuaValue vectorType = env.get("Vector3d");
        LuaValue ctor = vectorType.get("new");
        if (vectorType.istable() && ctor.isfunction()) {
            Varargs result = ctor.invoke(LuaValue.varargsOf(new LuaValue[]{
                    vectorType,
                    LuaValue.valueOf(vector.x()),
                    LuaValue.valueOf(vector.y()),
                    LuaValue.valueOf(vector.z())
            }));
            return result.arg1();
        }

        LuaValue fallback = tableOf();
        fallback.set("x", LuaValue.valueOf(vector.x()));
        fallback.set("y", LuaValue.valueOf(vector.y()));
        fallback.set("z", LuaValue.valueOf(vector.z()));
        return fallback;
    }

    @FunctionalInterface
    private interface VectorSupplier {
        Vector3dc get();
    }
}
