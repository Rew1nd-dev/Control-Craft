package com.verr1.controlcraft.foundation.cimulink.core.components.lua;

import com.verr1.controlcraft.foundation.cimulink.core.api.IBusAccess;
import com.verr1.controlcraft.foundation.cimulink.core.api.IWorldAccess;
import org.jetbrains.annotations.NotNull;
import org.luaj.vm2.LuaTable;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.lib.OneArgFunction;
import org.luaj.vm2.lib.ThreeArgFunction;
import org.luaj.vm2.lib.TwoArgFunction;

public class BusLib extends TwoArgFunction {

    private final IBusAccess access;

    public BusLib(@NotNull IBusAccess access) {
        this.access = access;
    }

    @Override
    public LuaValue call(LuaValue modname, LuaValue env) {
        LuaTable bus = new LuaTable();

        bus.set("propagate", new ThreeArgFunction() {
            @Override
            public LuaValue call(LuaValue arg0, LuaValue arg1, LuaValue arg2) {
                access.propagate(arg0.checkjstring(), arg1.checkjstring(), arg2.checkdouble());
                return LuaValue.NIL;
            }
        });

        bus.set("retrieve", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue arg0, LuaValue arg1) {
                return LuaValue.valueOf(access.retrieve(arg0.checkjstring(), arg1.checkjstring()));
            }
        });


        env.set("Bus", bus);
        env.get("package").get("loaded").set("Bus", bus);
        return bus;
    }
}
