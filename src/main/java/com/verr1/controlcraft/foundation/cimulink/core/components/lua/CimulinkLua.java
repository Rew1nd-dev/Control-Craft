package com.verr1.controlcraft.foundation.cimulink.core.components.lua;

import com.verr1.controlcraft.foundation.cimulink.core.components.luacuit.Luacuit;
import com.verr1.controlcraft.foundation.cimulink.core.components.luacuit.LuacuitConstructor;
import com.verr1.controlcraft.foundation.cimulink.core.components.luacuit.LuacuitScript;
import org.luaj.vm2.Globals;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.lib.DebugLib;
import org.luaj.vm2.lib.jse.JsePlatform;

public class CimulinkLua {



    public static final Luacuit EMPTY_LUACUIT = new LuacuitConstructor(LuacuitScript.EMPTY).build();

    public static Globals createStandardGlobals(){
        Globals g = JsePlatform.standardGlobals();
        loadLuaML(g);
        g.load(new WatcherLib());
        return g;
    }

    public static void loadLuaML(Globals globals){
        String luaMl = LuaScriptLoader.LUA_SCRIPTS.get("luaml.lua");
        if(luaMl != null){
            LuaValue module = globals.load(luaMl, "luaml.lua").call();
            if (module.istable()) {
                globals.set("Vector3d", module.get("Vector3d"));
                globals.set("Quaterniond", module.get("Quaterniond"));
            }
        }

    }

    public static boolean interrupt(Globals globals){
        DebugLib debugLib = globals.debuglib;
        if(debugLib instanceof WatcherLib watcher){
            watcher.interrupt();
            return true;
        }
        return false;
    }



}
