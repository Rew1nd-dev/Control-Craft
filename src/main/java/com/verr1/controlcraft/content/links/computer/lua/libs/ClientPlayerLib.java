package com.verr1.controlcraft.content.links.computer.lua.libs;

import net.minecraft.client.Minecraft;
import net.minecraft.client.player.LocalPlayer;
import org.joml.Vector3dc;
import org.luaj.vm2.LuaTable;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.Varargs;
import org.luaj.vm2.lib.TwoArgFunction;
import org.luaj.vm2.lib.VarArgFunction;
import org.luaj.vm2.lib.jse.CoerceJavaToLua;
import org.valkyrienskies.mod.common.VSGameUtilsKt;
import org.valkyrienskies.mod.common.entity.ShipMountedToData;

public class ClientPlayerLib extends TwoArgFunction {

    @Override
    public LuaValue call(LuaValue modname, LuaValue env) {
        LuaValue library = tableOf();

        library.set("yaw", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                LocalPlayer player = Minecraft.getInstance().player;
                if (player == null) {
                    return LuaValue.valueOf(0.0);
                }
                float partial = args.narg() >= 1 ? (float) args.checkdouble(1) : 0.0f;
                return LuaValue.valueOf(player.getViewYRot(partial));
            }
        });

        library.set("pitch", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                LocalPlayer player = Minecraft.getInstance().player;
                if (player == null) {
                    return LuaValue.valueOf(0.0);
                }
                float partial = args.narg() >= 1 ? (float) args.checkdouble(1) : 0.0f;
                return LuaValue.valueOf(player.getViewXRot(partial));
            }
        });

        library.set("getShipMountedToData", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                LocalPlayer player = Minecraft.getInstance().player;
                if (player == null) {
                    return NIL;
                }

                Float partialTicks = args.narg() >= 1 ? (float) args.checkdouble(1) : null;
                ShipMountedToData data = VSGameUtilsKt.getShipMountedToData(player, partialTicks);
                if (data == null) {
                    return NIL;
                }

                LuaTable table = new LuaTable();
                table.set("shipMountedTo", CoerceJavaToLua.coerce(data.getShipMountedTo()));
                table.set("shipMountedToId", LuaValue.valueOf(data.getShipMountedTo().getId()));
                table.set("mountPosInShip", toLuaVec(data.getMountPosInShip(), env));
                return table;
            }
        });

        env.set("Player", library);
        return library;
    }

    private static LuaValue toLuaVec(Vector3dc v, LuaValue env) {
        if (v == null) {
            return LuaValue.NIL;
        }

        LuaValue vec = env.get("Vector3d");
        if (!vec.isnil()) {
            LuaValue ctorMethod = vec.get("new");
            if (ctorMethod.isfunction()) {
                return ctorMethod.invoke(LuaValue.varargsOf(new LuaValue[]{
                        vec,
                        LuaValue.valueOf(v.x()),
                        LuaValue.valueOf(v.y()),
                        LuaValue.valueOf(v.z())
                })).arg1();
            }
            if (vec.isfunction()) {
                return vec.call(
                        LuaValue.valueOf(v.x()),
                        LuaValue.valueOf(v.y()),
                        LuaValue.valueOf(v.z()));
            }
        }

        LuaTable t = new LuaTable();
        t.set("x", LuaValue.valueOf(v.x()));
        t.set("y", LuaValue.valueOf(v.y()));
        t.set("z", LuaValue.valueOf(v.z()));
        return t;
    }
}
