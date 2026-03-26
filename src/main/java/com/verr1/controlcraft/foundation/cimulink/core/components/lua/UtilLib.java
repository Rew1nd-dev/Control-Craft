package com.verr1.controlcraft.foundation.cimulink.core.components.lua;

import com.verr1.controlcraft.foundation.cimulink.core.api.IWorldAccess;
import com.verr1.controlcraft.utils.AsyncDebugFileLogger;
import com.verr1.controlcraft.utils.GlobalTickClock;
import org.jetbrains.annotations.NotNull;
import org.luaj.vm2.LuaTable;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.lib.OneArgFunction;
import org.luaj.vm2.lib.TwoArgFunction;
import org.luaj.vm2.lib.VarArgFunction;
import org.luaj.vm2.Varargs;
import org.luaj.vm2.lib.ZeroArgFunction;

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

        util.set("debugLog", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                String fileName = args.narg() >= 2 ? args.checkjstring(1) : access.defaultDebugLogFile();
                String message = args.narg() >= 2 ? args.checkjstring(2) : args.checkjstring(1);
                return LuaValue.valueOf(access.debugLog(fileName, message));
            }
        });

        util.set("resetDebugLog", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                String fileName = args.narg() >= 1 ? args.checkjstring(1) : access.defaultDebugLogFile();
                return LuaValue.valueOf(access.resetDebugLog(fileName));
            }
        });

        util.set("getDebugLogPath", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                String fileName = args.narg() >= 1 ? args.checkjstring(1) : access.defaultDebugLogFile();
                return LuaValue.valueOf(AsyncDebugFileLogger.resolvePath(fileName).toString());
            }
        });

        util.set("beep", new org.luaj.vm2.lib.ThreeArgFunction() {
            @Override
            public LuaValue call(LuaValue arg1, LuaValue arg2, LuaValue arg3) {
                access.beep((float) arg1.checkdouble(), (float) arg2.checkdouble(), (float) arg3.checkdouble());
                return LuaValue.NIL;
            }
        });

        util.set("gameClock", new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                return LuaValue.valueOf(GlobalTickClock.gameClock());
            }
        });

        util.set("physClock", new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                return LuaValue.valueOf(GlobalTickClock.physClock());
            }
        });

        util.set("dbf", new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                double[] value = access.debug_lastTickFlapControls();
                LuaTable table = new LuaTable();
                table.set("fx", LuaValue.valueOf(value[0]));
                table.set("fy", LuaValue.valueOf(value[1]));
                table.set("fz", LuaValue.valueOf(value[2]));
                table.set("tx", LuaValue.valueOf(value[3]));
                table.set("ty", LuaValue.valueOf(value[4]));
                table.set("tz", LuaValue.valueOf(value[5]));
                return table;
            }
        });

        env.set("World", util);
        // env.get("package").get("loaded").set("World", util);
        return util;
    }
}
