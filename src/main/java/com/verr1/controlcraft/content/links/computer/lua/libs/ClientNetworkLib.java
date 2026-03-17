package com.verr1.controlcraft.content.links.computer.lua.libs;

import com.verr1.controlcraft.content.links.computer.ComputerNetworkHandler;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.lib.OneArgFunction;
import org.luaj.vm2.lib.TwoArgFunction;
import org.luaj.vm2.lib.jse.CoerceJavaToLua;

public class ClientNetworkLib extends TwoArgFunction {

    private final ComputerNetworkHandler syncManager;
    public ClientNetworkLib(ComputerNetworkHandler syncManager) {
        this.syncManager = syncManager;
    }
    @Override
    public LuaValue call(LuaValue modname, LuaValue env) {
        LuaValue library = tableOf();
        // sync.isDirty("slot_name") -> boolean
        library.set("isDirty", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue slot) {
                return LuaValue.valueOf(syncManager.isDirty(slot.checkjstring()));
            }
        });
        // sync.peek("slot_name") -> value
        library.set("peek", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue slot) {
                Object val = syncManager.peek(slot.checkjstring());
                return val == null ? NIL : CoerceJavaToLua.coerce(val);
            }
        });
        // sync.retrieve("slot_name") -> value (并清除 dirty 标记)
        library.set("retrieve", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue slot) {
                Object val = syncManager.retrieve(slot.checkjstring());
                return val == null ? NIL : CoerceJavaToLua.coerce(val);
            }
        });
        env.set("Network", library);
        return library;
    }

}
