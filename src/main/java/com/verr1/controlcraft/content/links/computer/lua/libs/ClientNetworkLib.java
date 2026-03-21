package com.verr1.controlcraft.content.links.computer.lua.libs;

import com.verr1.controlcraft.content.links.computer.ComputerNetworkHandler;
import com.verr1.controlcraft.content.links.computer.lua.IComputerClientContext;
import com.verr1.controlcraft.foundation.network.packets.specific.ComputerClientNetworkPacket;
import com.verr1.controlcraft.registry.ControlCraftPackets;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.lib.OneArgFunction;
import org.luaj.vm2.lib.TwoArgFunction;
import org.luaj.vm2.lib.jse.CoerceJavaToLua;
import org.luaj.vm2.lib.jse.CoerceLuaToJava;

public class ClientNetworkLib extends TwoArgFunction {

    private final IComputerClientContext context;
    private final ComputerNetworkHandler syncManager;

    public ClientNetworkLib(IComputerClientContext context) {
        this.context = context;
        this.syncManager = context.getNetworkHandler();
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

        library.set("send", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue slot, LuaValue value) {
                String slotName = slot.checkjstring();
                Object javaValue = CoerceLuaToJava.coerce(value, Object.class);
                ControlCraftPackets.getChannel().sendToServer(
                        new ComputerClientNetworkPacket(context.getBlockPos(), slotName, javaValue)
                );
                return TRUE;
            }
        });

        env.set("Network", library);
        return library;
    }
}
