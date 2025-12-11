package com.verr1.controlcraft.foundation.cimulink.core.components.luacuit;

import com.mojang.datafixers.util.Either;
import com.verr1.controlcraft.foundation.cimulink.core.components.lua.CimulinkLua;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.LuaOvertimeException;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.UndefineMethodException;
import org.luaj.vm2.Globals;
import org.luaj.vm2.LuaError;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.lib.OneArgFunction;
import org.luaj.vm2.lib.jse.JsePlatform;

import javax.naming.CommunicationException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicReference;

public class LuacuitConstructor {

    private final LuacuitScript script;

    public LuacuitConstructor(LuacuitScript script){
        this.script = script;
    }

    public LuacuitConstructor(String code) throws LuaOvertimeException, LuaError{
        this.script = LuacuitScript.fromCode(code);
    }



    public Luacuit build() throws UndefineMethodException, LuaError, LuaOvertimeException{

        Globals luaGlobal = CimulinkLua.createStandardGlobals();

        CompletableFuture<LuaValue> future = CompletableFuture.supplyAsync(() -> {
            LuaValue chunk = luaGlobal.load(script.code());
            chunk.call();
            return luaGlobal.get("loop");

        })
            .completeOnTimeout(LuaValue.NIL, 1000, TimeUnit.MILLISECONDS);

        LuaValue loopFunc;
        try{
            loopFunc = future.join();
            if(loopFunc == LuaValue.NIL){
                if(future.isDone()){
                    throw new UndefineMethodException("loop() is not present!");
                }else{
                    throw new LuaOvertimeException("Compiling Lua script overtime!");
                }
            }
        } catch (CompletionException e) {
            Throwable e0 = e.getCause();
            if(e0 instanceof LuaError luaError){
                throw luaError;
            }
            throw new RuntimeException(e);
        }

        return new Luacuit(this.script.definedInputs(), this.script.definedOutputs(), luaGlobal, loopFunc, script);
    }

}
