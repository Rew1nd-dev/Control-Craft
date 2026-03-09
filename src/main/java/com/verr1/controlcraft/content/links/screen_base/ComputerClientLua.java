package com.verr1.controlcraft.content.links.screen_base;

import com.verr1.controlcraft.content.links.screen_base.lua.IComputerClientContext;
import com.verr1.controlcraft.content.links.screen_base.lua.IComputerClientHandler;
import com.verr1.controlcraft.content.links.screen_base.lua.LuaUtils;
import com.verr1.controlcraft.content.links.screen_base.lua.render.RenderLib;
import com.verr1.controlcraft.foundation.cimulink.core.components.lua.PhysLib;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.LuaOvertimeException;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.UndefineMethodException;
import org.jetbrains.annotations.Nullable;
import org.luaj.vm2.Globals;
import org.luaj.vm2.LuaError;
import org.luaj.vm2.LuaValue;

import static com.verr1.controlcraft.content.links.screen_base.ComputerServerLua.createStandardGlobals;

public class ComputerClientLua extends ComputerCommonLua {

    private final IComputerClientContext context;

    protected final @Nullable LuaValue loopFunction;

    private ComputerClientLua(
            IComputerClientContext context,
            Globals luaGlobals,
            @Nullable LuaValue loopFunction) {
        super(luaGlobals);
        this.context = context;
        this.loopFunction = loopFunction;
    }

    public static ComputerClientLua fromCode(
            IComputerClientContext context,
            String code)
            throws UndefineMethodException, LuaError, LuaOvertimeException {
        Globals luaGlobal = createStandardGlobals();

        luaGlobal.load(new PhysLib(context.getPhysAccess()));

        RenderLib renderLib = new RenderLib(context.getScreen());
        luaGlobal.load(renderLib);

        LuaValue loopFunc = LuaUtils.safeLoadValue(code, "onClientTick", luaGlobal).filter(LuaValue::isfunction).orElse(null);

        return new ComputerClientLua(context, luaGlobal, loopFunc);
    }

    public void onClientTick() {
        if (loopFunction == null)
            return;
        runWithProtection(loopFunction::call);
    }
}
