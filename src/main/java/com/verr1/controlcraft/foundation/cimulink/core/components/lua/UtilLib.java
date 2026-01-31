package com.verr1.controlcraft.foundation.cimulink.core.components.lua;

import com.verr1.controlcraft.foundation.cimulink.core.api.IWorldAccess;
import org.jetbrains.annotations.NotNull;
import org.luaj.vm2.LuaTable;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.lib.OneArgFunction;
import org.luaj.vm2.lib.TwoArgFunction;

public class UtilLib extends TwoArgFunction {

    private final IWorldAccess access;

    public UtilLib(@NotNull IWorldAccess access) {
        this.access = access;
    }

    @Override
    public LuaValue call(LuaValue modname, LuaValue env) {
        LuaTable util = new LuaTable();

        util.set("yell", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue arg0, LuaValue arg1) {
                access.yell((float)arg0.checkdouble(), arg1.checkjstring());
                return LuaValue.NIL;
            }
        });

        util.set("log", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue arg) {
                access.log(arg.checkjstring());
                return LuaValue.NIL;
            }
        });

        util.set("beep", new org.luaj.vm2.lib.ThreeArgFunction() {
            @Override
            public LuaValue call(LuaValue arg1, LuaValue arg2, LuaValue arg3) {
                access.beep((float) arg1.checkdouble(), (float) arg2.checkdouble(), (float) arg3.checkdouble());
                return LuaValue.NIL;
            }
        });

        env.set("World", util);
        env.get("package").get("loaded").set("World", util);
        return util;
    }
}
