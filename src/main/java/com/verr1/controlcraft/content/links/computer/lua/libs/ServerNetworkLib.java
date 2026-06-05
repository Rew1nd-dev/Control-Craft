package com.verr1.controlcraft.content.links.computer.lua.libs;

import com.verr1.controlcraft.content.links.computer.ComputerNetworkHandler;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.lib.OneArgFunction;
import org.luaj.vm2.lib.TwoArgFunction;
import org.luaj.vm2.lib.jse.CoerceJavaToLua;
import org.luaj.vm2.lib.jse.CoerceLuaToJava;

public class ServerNetworkLib extends TwoArgFunction {

    private final ComputerNetworkHandler syncManager;

    public ServerNetworkLib(ComputerNetworkHandler syncManager) {
        this.syncManager = syncManager;
    }

    @Override
    public LuaValue call(LuaValue modname, LuaValue env) {
        LuaValue library = tableOf();

        library.set("isDirty", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue slot) {
                return LuaValue.valueOf(syncManager.isDirty(slot.checkjstring()));
            }
        });

        library.set("peek", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue slot) {
                Object val = syncManager.peek(slot.checkjstring());
                return val == null ? NIL : CoerceJavaToLua.coerce(val);
            }
        });

        library.set("retrieve", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue slot) {
                Object val = syncManager.retrieve(slot.checkjstring());
                return val == null ? NIL : CoerceJavaToLua.coerce(val);
            }
        });

        library.set("set", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue slot, LuaValue value) {
                String slotName = slot.checkjstring();
                Object javaValue = CoerceLuaToJava.coerce(value, Object.class);
                syncManager.set(slotName, javaValue);
                return NIL;
            }
        });

        env.set("Network", library);
        return library;
    }
}