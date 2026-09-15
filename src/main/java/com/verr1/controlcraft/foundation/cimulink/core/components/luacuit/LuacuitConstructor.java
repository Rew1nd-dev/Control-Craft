package com.verr1.controlcraft.foundation.cimulink.core.components.luacuit;

import com.verr1.controlcraft.content.links.computer.lua.LuaUtils;
import com.verr1.controlcraft.foundation.cimulink.core.components.lua.CimulinkLua;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.LuaOvertimeException;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.UndefineMethodException;
import org.luaj.vm2.Globals;
import org.luaj.vm2.LuaError;
import org.luaj.vm2.LuaValue;

public class LuacuitConstructor {

    public static int LOAD_MAX_INSTRUCTIONS = LuaUtils.DEFAULT_LOAD_MAX_INSTRUCTIONS;
    public static long LOAD_MAX_TIME_MILLIS = LuaUtils.DEFAULT_LOAD_MAX_TIME_MILLIS;
    public static long LOAD_MAX_ALLOCATED_BYTES = LuaUtils.DEFAULT_LOAD_MAX_ALLOCATED_BYTES;

    private final LuacuitScript script;

    public LuacuitConstructor(LuacuitScript script) {
        this.script = script;
    }

    public LuacuitConstructor(String code) throws LuaOvertimeException, LuaError {
        this.script = LuacuitScript.fromCode(code);
    }

    public Luacuit build() throws UndefineMethodException, LuaError, LuaOvertimeException {
        return build(LOAD_MAX_INSTRUCTIONS, LOAD_MAX_TIME_MILLIS, LOAD_MAX_ALLOCATED_BYTES);
    }

    public Luacuit build(
            int loadMaxInstructions,
            long loadMaxTimeMillis,
            long loadMaxAllocatedBytes
    ) throws UndefineMethodException, LuaError, LuaOvertimeException {
        Globals luaGlobal = CimulinkLua.createStandardGlobals();
        LuaValue loopFunc = LuaUtils.loadFunctionBlock(
                script.code(),
                "loop",
                luaGlobal,
                loadMaxInstructions,
                loadMaxTimeMillis,
                loadMaxAllocatedBytes
        );

        return new Luacuit(this.script.definedInputs(), this.script.definedOutputs(), luaGlobal, loopFunc, script);
    }
}
