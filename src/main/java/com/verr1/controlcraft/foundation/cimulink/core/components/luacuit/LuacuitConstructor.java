package com.verr1.controlcraft.foundation.cimulink.core.components.luacuit;

import com.verr1.controlcraft.foundation.cimulink.core.components.lua.CimulinkLua;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.LuaOvertimeException;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.UndefineMethodException;
import org.luaj.vm2.Globals;
import org.luaj.vm2.LuaError;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.lib.OneArgFunction;
import org.luaj.vm2.lib.jse.JsePlatform;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicReference;

public class LuacuitConstructor {

    private final LuacuitScript script;

    public LuacuitConstructor(LuacuitScript script){
        this.script = script;
    }

    public LuacuitConstructor(String code) throws LuaOvertimeException, LuaError{
        LuacuitScript temporary = LuacuitScript.EMPTY;
        Globals defineGlobal = CimulinkLua.createStandardGlobals();

        List<String> collectedInputs = new ArrayList<>();
        List<String> collectedOutputs = new ArrayList<>();

        defineGlobal.set("defineInput", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue arg) {
                collectedInputs.add(arg.checkjstring());
                return LuaValue.NIL;
            }
        });

        defineGlobal.set("defineOutput", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue arg) {
                collectedOutputs.add(arg.checkjstring());
                return LuaValue.NIL;
            }
        });


        CompletableFuture<Void> future = CompletableFuture.runAsync(() -> {
            LuaValue chunk = defineGlobal.load(code);
            chunk.call();
            LuaValue defineFunc = defineGlobal.get("define");
            if(defineFunc == LuaValue.NIL)return;
            defineFunc.call();
        });

        try{
            future.get(3, TimeUnit.MILLISECONDS);
        } catch (ExecutionException | InterruptedException e) {
            throw new RuntimeException(e);
        } catch (TimeoutException e) {
            throw new LuaOvertimeException(e.getMessage());
        }


        temporary = new LuacuitScript(
                code,
                collectedInputs,
                collectedOutputs
        );

        this.script = temporary;
    }

    public Luacuit build() throws UndefineMethodException, LuaError, LuaOvertimeException{

        Globals luaGlobal = CimulinkLua.createStandardGlobals();

        CompletableFuture<LuaValue> future = CompletableFuture.supplyAsync(() -> {
            LuaValue chunk = luaGlobal.load(script.code());
            chunk.call();
            return luaGlobal.get("loop");
        });
        LuaValue loopFunc;
        try{
            loopFunc = future.get(3, TimeUnit.MILLISECONDS);
            if(loopFunc == LuaValue.NIL){
                throw new UndefineMethodException("loop() is not present!");
            }
        } catch (ExecutionException e) {
            Throwable e0 = e.getCause();
            if(e0 instanceof LuaError luaError){
                throw luaError;
            }
            throw new RuntimeException(e);
        } catch (TimeoutException e) {
            throw new LuaOvertimeException("Lua Code Execution Overtime");
        } catch (InterruptedException e){
            throw new RuntimeException(e);
        }

        return new Luacuit(this.script.definedInputs(), this.script.definedOutputs(), luaGlobal, loopFunc, script);
    }

}
