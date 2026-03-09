package com.verr1.controlcraft.foundation.cimulink.core.components.luacuit;

import com.verr1.controlcraft.content.links.screen_base.lua.LuaUtils;
import com.verr1.controlcraft.foundation.cimulink.core.components.lua.CimulinkLua;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.LuaOvertimeException;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.UndefineMethodException;
import org.luaj.vm2.Globals;
import org.luaj.vm2.LuaError;
import org.luaj.vm2.LuaValue;

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
//        CompletableFuture<LuaValue> future = CompletableFuture.supplyAsync(() -> {
//            LuaValue chunk = luaGlobal.load(script.code());
//            chunk.call();
//            return luaGlobal.get("loop");
//
//        })
//            .completeOnTimeout(LuaValue.NIL, 1000, TimeUnit.MILLISECONDS);
//
//        LuaValue loopFunc;
//        try{
//            loopFunc = future.join();
//            if(loopFunc == LuaValue.NIL){
//                if(future.isDone()){
//                    throw new UndefineMethodException("loop() is not present!");
//                }else{
//                    throw new LuaOvertimeException("Compiling Lua script overtime!");
//                }
//            }
//        } catch (CompletionException e) {
//            Throwable e0 = e.getCause();
//            if(e0 instanceof LuaError luaError){
//                throw luaError;
//            }
//            throw new RuntimeException(e);
//        }
        LuaValue loopFunc = LuaUtils.loadFunctionBlock(script.code(), "loop", luaGlobal);


        return new Luacuit(this.script.definedInputs(), this.script.definedOutputs(), luaGlobal, loopFunc, script);
    }

}
