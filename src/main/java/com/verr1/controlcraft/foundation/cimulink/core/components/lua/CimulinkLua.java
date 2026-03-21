package com.verr1.controlcraft.foundation.cimulink.core.components.lua;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.cimulink.core.components.luacuit.Luacuit;
import com.verr1.controlcraft.foundation.cimulink.core.components.luacuit.LuacuitConstructor;
import com.verr1.controlcraft.foundation.cimulink.core.components.luacuit.LuacuitScript;
import org.luaj.vm2.Globals;
import org.luaj.vm2.LoadState;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.compiler.LuaC;
import org.luaj.vm2.lib.*;
import org.luaj.vm2.lib.jse.*;

public class CimulinkLua {



    public static final Luacuit EMPTY_LUACUIT = new LuacuitConstructor(LuacuitScript.EMPTY).build();

    public static Globals createStandardGlobals(){
        Globals g = createJseStandardGlobals();//JsePlatform.standardGlobals();
        loadLuaML(g);
        g.load(new WatcherLib());
        return g;
    }

    public static Globals createJseStandardGlobals(){
        Globals var0 = new Globals();
        var0.load(new BaseLib());
        var0.set("dofile", LuaValue.NIL);
        var0.set("loadfile", LuaValue.NIL);
        var0.set("load", LuaValue.NIL);

        var0.load(new PackageLib());
        var0.load(new Bit32Lib());
        var0.load(new TableLib());
        var0.load(new StringLib());
        var0.load(new CoroutineLib());
        var0.load(new JseMathLib());
        LoadState.install(var0);
        LuaC.install(var0);
        return var0;
    }

    public static void loadLuaML(Globals globals){
        String luaMl = LuaScriptLoader.getScript("luaml.lua");
        if (luaMl == null || luaMl.isBlank()) {
            ControlCraft.LOGGER.warn("luaml.lua is unavailable, Vector3d/Quaterniond will not be registered.");
            return;
        }

        LuaValue module = globals.load(luaMl, "luaml.lua").call();
        if (module.istable()) {
            globals.set("Vector3d", module.get("Vector3d"));
            globals.set("Quaterniond", module.get("Quaterniond"));
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
