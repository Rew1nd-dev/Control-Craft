package com.verr1.controlcraft.content.links.computer;

import com.verr1.controlcraft.content.links.computer.lua.IComputerServerContext;
import com.verr1.controlcraft.content.links.computer.lua.libs.BlockLib;
import com.verr1.controlcraft.content.links.computer.lua.LuaUtils;
import com.verr1.controlcraft.content.links.computer.lua.libs.ComputerWatcherLib;
import com.verr1.controlcraft.content.links.computer.lua.libs.ServerNetworkLib;
import com.verr1.controlcraft.foundation.cimulink.core.components.lua.PhysLib;
import com.verr1.controlcraft.foundation.cimulink.core.components.lua.UtilLib;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.LuaOvertimeException;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.UndefineMethodException;
import org.jetbrains.annotations.Nullable;
import org.luaj.vm2.*;
import org.luaj.vm2.compiler.LuaC;
import org.luaj.vm2.lib.*;
import org.luaj.vm2.lib.jse.JseMathLib;

import java.util.Map;

import static com.verr1.controlcraft.foundation.cimulink.core.components.lua.CimulinkLua.loadLuaML;

public class ComputerServerLua extends ComputerCommonLua{

    public static int MAX_ALLOCATED_BYTES = 128 * 1024;
    public static int MAX_INSTRUCTIONS = 1_000_0;
    public static int MAX_TIME_MILLIS = 2;

    private final IComputerServerContext context;

    protected final @Nullable LuaValue loopFunction;
    protected final @Nullable LuaValue playerEventHandler;

    private ComputerServerLua(
        IComputerServerContext context,
        Globals luaGlobals,
        @Nullable LuaValue loopFunction,
        @Nullable LuaValue playerEventFunc
    ) {
        super(luaGlobals);
        this.context = context;
        this.loopFunction = loopFunction;
        this.playerEventHandler = playerEventFunc;
    }

    public static ComputerServerLua fromCode(
        IComputerServerContext context,
        String code
    )
        throws UndefineMethodException, LuaError, LuaOvertimeException
    {
        Globals luaGlobal = createStandardGlobals();

        luaGlobal.load(new PhysLib(context.getPhysAccess()));
        luaGlobal.load(new UtilLib(context.getWorldAccess()));
        luaGlobal.load(new BlockLib(context));
        luaGlobal.load(new ServerNetworkLib(context.getNetworkHandler()));
        loadLuaML(luaGlobal);
        LuaValue loopFunc  = LuaUtils.safeLoadValue(code, "onServerTick",  luaGlobal).filter(LuaValue::isfunction).orElse(null);
        LuaValue playerEventFunc = LuaUtils.safeLoadValue(code, "onPlayerEvent", luaGlobal).filter(LuaValue::isfunction).orElse(null);


        return new ComputerServerLua(context, luaGlobal, loopFunc, playerEventFunc);
    }


    @Override
    protected void onError(LuaError e) {
        context.onError(e);
    }


    public void onPlayerTouch(int x, int y){
        if(playerEventHandler == null)return;
        LuaTable event = LuaUtils.makeTable(Map.of(
            "type", "touch",
            "x", x,
            "y", y
        ));

        runWithProtection(() -> playerEventHandler.call(event), MAX_INSTRUCTIONS, MAX_TIME_MILLIS, MAX_ALLOCATED_BYTES);
    }


    public void onPlayerWatch(int x, int y) {
        if(playerEventHandler == null)return;
        LuaTable event = LuaUtils.makeTable(Map.of(
            "type", "watch",
            "x", x,
            "y", y
        ));

        runWithProtection(() -> playerEventHandler.call(event), MAX_INSTRUCTIONS, MAX_TIME_MILLIS, MAX_ALLOCATED_BYTES);
    }


    public void onServerTick() {
        if(loopFunction == null)return;
        runWithProtection(loopFunction::call, MAX_INSTRUCTIONS, MAX_TIME_MILLIS, MAX_ALLOCATED_BYTES);
    }


    public static Globals createStandardGlobals() {
        Globals g = createJseStandardGlobals();// JsePlatform.standardGlobals();
        loadLuaML(g);
        g.load(new ComputerWatcherLib());

        return g;
    }

    public static Globals createJseStandardGlobals() {
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
}
