package com.verr1.controlcraft.content.links.computer;

import com.verr1.controlcraft.content.links.computer.lua.IComputerClientContext;
import com.verr1.controlcraft.content.links.computer.lua.LuaUtils;
import com.verr1.controlcraft.content.links.computer.lua.libs.BlockLib;
import com.verr1.controlcraft.content.links.computer.lua.libs.ClientNetworkLib;
import com.verr1.controlcraft.content.links.computer.lua.libs.ClientPlayerLib;
import com.verr1.controlcraft.content.links.computer.lua.libs.ClientShipLib;
import com.verr1.controlcraft.content.links.computer.lua.libs.RenderLib;
import com.verr1.controlcraft.content.links.computer.lua.libs.TextUtilLib;
import com.verr1.controlcraft.foundation.cimulink.core.components.lua.CimulinkLua;
import com.verr1.controlcraft.foundation.cimulink.core.components.lua.PhysLib;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.LuaOvertimeException;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.UndefineMethodException;
import org.jetbrains.annotations.Nullable;
import org.luaj.vm2.Globals;
import org.luaj.vm2.LuaError;
import org.luaj.vm2.LuaValue;

import static com.verr1.controlcraft.foundation.cimulink.core.components.lua.CimulinkLua.loadLuaML;

public class ComputerClientLua extends ComputerCommonLua {

    public static int LOAD_MAX_INSTRUCTIONS = LuaUtils.DEFAULT_LOAD_MAX_INSTRUCTIONS;
    public static long LOAD_MAX_TIME_MILLIS = LuaUtils.DEFAULT_LOAD_MAX_TIME_MILLIS;
    public static long LOAD_MAX_ALLOCATED_BYTES = LuaUtils.DEFAULT_LOAD_MAX_ALLOCATED_BYTES;

    private final IComputerClientContext context;

    protected final @Nullable LuaValue loopFunction;

    private ComputerClientLua(
            IComputerClientContext context,
            Globals luaGlobals,
            @Nullable LuaValue loopFunction
    ) {
        super(luaGlobals);
        this.context = context;
        this.loopFunction = loopFunction;
    }

    public static ComputerClientLua fromCode(
            IComputerClientContext context,
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

    public static ComputerClientLua fromCode(
            IComputerClientContext context,
            String code,
            int loadMaxInstructions,
            long loadMaxTimeMillis,
            long loadMaxAllocatedBytes
    ) throws UndefineMethodException, LuaError, LuaOvertimeException {
        Globals luaGlobal = CimulinkLua.createJseStandardGlobals();

        luaGlobal.load(new PhysLib(context.getPhysAccess()));
        luaGlobal.load(new RenderLib(context.getScreen()));
        luaGlobal.load(new BlockLib(context));
        luaGlobal.load(new ClientPlayerLib(context));
        luaGlobal.load(new ClientShipLib());
        luaGlobal.load(new ClientNetworkLib(context));
        luaGlobal.load(new TextUtilLib());
        loadLuaML(luaGlobal);

        LuaValue loopFunc = LuaUtils.safeLoadValue(
                code,
                "onClientTick",
                luaGlobal,
                loadMaxInstructions,
                loadMaxTimeMillis,
                loadMaxAllocatedBytes
        ).filter(LuaValue::isfunction).orElse(null);

        return new ComputerClientLua(context, luaGlobal, loopFunc);
    }

    public void onClientTick() {
        if (loopFunction == null) {
            return;
        }
        runWithProtection(loopFunction::call, 450000, 20 * 4, 1024 * 1024);
    }
}
