package com.verr1.controlcraft.content.links.computer.lua;

import com.verr1.controlcraft.foundation.cimulink.game.exceptions.LuaOvertimeException;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.UndefineMethodException;
import org.luaj.vm2.Globals;
import org.luaj.vm2.LuaError;
import org.luaj.vm2.LuaTable;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.lib.jse.CoerceJavaToLua;

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.TimeUnit;

public class LuaUtils {

    public static LuaTable makeTable(Map<String, Object> map) {
        LuaTable table = new LuaTable();
        for (Map.Entry<String, Object> entry : map.entrySet()) {
            table.set(entry.getKey(), CoerceJavaToLua.coerce(entry.getValue()));
        }
        return table;
    }

    public static LuaValue loadFunctionBlock(String code, String functionName, Globals luaGlobal)
        throws UndefineMethodException, LuaOvertimeException, CompletionException
    {

        CompletableFuture<LuaValue> future = CompletableFuture.supplyAsync(() -> {
                LuaValue chunk = luaGlobal.load(code);
                chunk.call();
                return luaGlobal.get(functionName);

            })
            .completeOnTimeout(LuaValue.NIL, 1000, TimeUnit.MILLISECONDS);

        LuaValue loopFunc;
        try{
            loopFunc = future.join();
            if(loopFunc == LuaValue.NIL){
                if(future.isDone()){
                    throw new UndefineMethodException(functionName + " is not present!");
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

        return loopFunc;
    }

    public static Optional<LuaValue> safeLoadValue(String code, String functionName, Globals luaGlobal){
        try{
            return Optional.of(loadFunctionBlock(code, functionName, luaGlobal));
        } catch (Exception e) {
            return Optional.empty();
        }
    }

}
