package com.verr1.controlcraft.foundation.cimulink.core.components.lua;

import org.luaj.vm2.Globals;
import org.luaj.vm2.lib.DebugLib;
import org.luaj.vm2.lib.jse.JsePlatform;

public class CimulinkLua {
    public static final String EMPTY_CODE =
            """
            function define() end
            function loop() end
            """;

    public static Globals createStandardGlobals(){
        Globals g = JsePlatform.standardGlobals();
        g.load(new WatcherLib());
        return g;
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
