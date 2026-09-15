package com.verr1.controlcraft.content.links.computer;

import com.verr1.controlcraft.content.links.computer.lua.IComputerServerContext;
import com.verr1.controlcraft.content.links.computer.lua.LuaUtils;
import com.verr1.controlcraft.content.links.computer.lua.libs.BlockLib;
import com.verr1.controlcraft.content.links.computer.lua.libs.ComputerWatcherLib;
import com.verr1.controlcraft.content.links.computer.lua.libs.LuaToComputerLib;
import com.verr1.controlcraft.content.links.computer.lua.libs.ServerNetworkLib;
import com.verr1.controlcraft.content.links.computer.lua.libs.ServerPlantLib;
import com.verr1.controlcraft.content.links.computer.lua.libs.TextUtilLib;
import com.verr1.controlcraft.foundation.cimulink.core.components.lua.BusLib;
import com.verr1.controlcraft.foundation.cimulink.core.components.lua.PhysLib;
import com.verr1.controlcraft.foundation.cimulink.core.components.lua.UtilLib;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.LuaOvertimeException;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.UndefineMethodException;
import org.jetbrains.annotations.Nullable;
import org.luaj.vm2.Globals;
import org.luaj.vm2.LoadState;
import org.luaj.vm2.LuaError;
import org.luaj.vm2.LuaTable;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.compiler.LuaC;
import org.luaj.vm2.lib.BaseLib;
import org.luaj.vm2.lib.Bit32Lib;
import org.luaj.vm2.lib.CoroutineLib;
import org.luaj.vm2.lib.PackageLib;
import org.luaj.vm2.lib.StringLib;
import org.luaj.vm2.lib.TableLib;
import org.luaj.vm2.lib.jse.JseMathLib;

import java.util.Map;

import static com.verr1.controlcraft.foundation.cimulink.core.components.lua.CimulinkLua.loadLuaML;

public class ComputerServerLua extends ComputerCommonLua {

    public static int LOAD_MAX_INSTRUCTIONS = LuaUtils.DEFAULT_LOAD_MAX_INSTRUCTIONS;
    public static long LOAD_MAX_TIME_MILLIS = LuaUtils.DEFAULT_LOAD_MAX_TIME_MILLIS;
    public static long LOAD_MAX_ALLOCATED_BYTES = LuaUtils.DEFAULT_LOAD_MAX_ALLOCATED_BYTES;

    public static int MAX_ALLOCATED_BYTES = 128 * 1024;
    public static int MAX_INSTRUCTIONS = 1_000_0;
    public static int MAX_TIME_MILLIS = 10;

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
    ) throws UndefineMethodException, LuaError, LuaOvertimeException {
        return fromCode(
                context,
                code,
                LOAD_MAX_INSTRUCTIONS,
                LOAD_MAX_TIME_MILLIS,
                LOAD_MAX_ALLOCATED_BYTES
        );
    }

    public static ComputerServerLua fromCode(
            IComputerServerContext context,
            String code,
            int loadMaxInstructions,
            long loadMaxTimeMillis,
            long loadMaxAllocatedBytes
    ) throws UndefineMethodException, LuaError, LuaOvertimeException {
        Globals luaGlobal = createStandardGlobals();

        luaGlobal.load(new PhysLib(context.getPhysAccess()));
        luaGlobal.load(new UtilLib(context.getWorldAccess()));
        luaGlobal.load(new BlockLib(context));
        luaGlobal.load(new BusLib(context.getCimulinkBus()));
        luaGlobal.load(new LuaToComputerLib(context.getWorldAccess()));
        luaGlobal.load(new ServerNetworkLib(context));
        luaGlobal.load(new ServerPlantLib(context));
        luaGlobal.load(new TextUtilLib());
        loadLuaML(luaGlobal);

        LuaValue loopFunc = LuaUtils.safeLoadValue(
                code,
                "onServerTick",
                luaGlobal,
                loadMaxInstructions,
                loadMaxTimeMillis,
                loadMaxAllocatedBytes
        ).filter(LuaValue::isfunction).orElse(null);
        LuaValue playerEventFunc = LuaUtils.safeLoadValue(
                code,
                "onPlayerEvent",
                luaGlobal,
                loadMaxInstructions,
                loadMaxTimeMillis,
                loadMaxAllocatedBytes
        ).filter(LuaValue::isfunction).orElse(null);

        return new ComputerServerLua(context, luaGlobal, loopFunc, playerEventFunc);
    }

    @Override
    protected void onError(LuaError e) {
        context.onError(e);
    }

    public void onPlayerTouch(int x, int y) {
        if (playerEventHandler == null) {
            return;
        }
        LuaTable event = LuaUtils.makeTable(Map.of(
                "type", "touch",
                "x", x,
                "y", y
        ));

        runWithProtection(() -> playerEventHandler.call(event),
            MAX_INSTRUCTIONS, MAX_TIME_MILLIS, MAX_ALLOCATED_BYTES
        );
    }

    public void onPlayerWatch(int x, int y) {
        if (playerEventHandler == null) {
            return;
        }
        LuaTable event = LuaUtils.makeTable(Map.of(
                "type", "watch",
                "x", x,
                "y", y
        ));

        runWithProtection(() -> playerEventHandler.call(event),
            MAX_INSTRUCTIONS, MAX_TIME_MILLIS, MAX_ALLOCATED_BYTES
        );
    }

    public void onServerTick() {
        if (loopFunction == null) {
            return;
        }
        runWithProtection(loopFunction::call,
            MAX_INSTRUCTIONS, MAX_TIME_MILLIS, MAX_ALLOCATED_BYTES
        );
    }

    public static Globals createStandardGlobals() {
        Globals g = createJseStandardGlobals();
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
