package com.verr1.controlcraft.foundation.cimulink.core.components.luacuit;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.lua.CimulinkLua;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.LuaOvertimeException;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.UnpresentPortException;
import net.minecraft.nbt.CompoundTag;
import org.luaj.vm2.Globals;
import org.luaj.vm2.LuaError;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.lib.TwoArgFunction;

import java.util.List;
import java.util.concurrent.*;

public class Luacuit extends NamedComponent{

    public static final ExecutorService LUA_THREAD = Executors.newSingleThreadExecutor();

    protected final Globals luaGlobals;
    protected final LuaValue loopFunction;
    protected final LuacuitScript script;

    protected boolean forbidden = false;

    Luacuit(
            List<String> inputs,
            List<String> outputs,
            Globals luaGlobals,
            LuaValue loopFunction,
            LuacuitScript script
    ) {
        super(inputs, outputs);
        this.luaGlobals = luaGlobals;
        this.loopFunction = loopFunction;
        this.script = script;
        luaGlobals.set("getInput", createBuiltInInput());
        luaGlobals.set("setOutput", createBuiltInOutput());
    }

    protected void outputToJava(String name, double value) throws LuaError {
        try{
            updateOutput(out(name), value);
        }catch (IllegalArgumentException e){
            throw new UnpresentPortException(e.getMessage());
        }

    }

    protected double inputFromJava(String name) throws LuaError{
        try{
            return retrieveInput(name);
        }catch (IllegalArgumentException e){
            throw new UnpresentPortException(e.getMessage());
        }

    }

    public CompoundTag serialize(){
        return script.serialize();
    }

    public static Luacuit deserialize(CompoundTag tag){
        LuacuitScript script = LuacuitScript.deserialize(tag);
        return new LuacuitConstructor(script).build();
    }

    public void forbid(){
        forbidden = true;
    }

    protected TwoArgFunction createBuiltInInput(){
        return new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue luaValue, LuaValue luaValue1){
                if (luaValue1.isnil()) {  // 如果是 getInput("name")
                    String name = luaValue.checkjstring();
                    return LuaValue.valueOf(inputFromJava(name));
                } else {
                    throw new LuaError("getInput expects one string argument");
                }
            }
        };
    }

    protected TwoArgFunction createBuiltInOutput(){
        return new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue luaValue, LuaValue luaValue1) {
                String name = luaValue.checkjstring();
                double value = luaValue1.checkdouble();
                outputToJava(name, value);
                return LuaValue.NIL;
            }
        };
    }

    @Override
    public List<Integer> propagateTo(int inputIndex) {
        return List.of();
    }

    @Override
    public void onInputChange(Integer... inputIndexes) {

    }

    @Override
    public void onPositiveEdge() throws LuaError, LuaOvertimeException {
        if(forbidden)return;

        Future<Void> future = LUA_THREAD.submit(() -> {
            loopFunction.call();
            return null;
        });

        try {
            future.get(16, TimeUnit.MILLISECONDS);  // 超时 16ms
        } catch (TimeoutException e) {
            future.cancel(true);
            boolean interrupted = CimulinkLua.interrupt(luaGlobals);
            if(!interrupted){
                throw new RuntimeException("Cannot Interrupt A Running Lua Global Because It Is Not Interruptible");
            }
            forbid();
            throw new LuaOvertimeException("Too Long Without Yielding");
        } catch (ExecutionException e) {
            if (e.getCause() instanceof LuaError le) {
                forbid();
                throw le;
            }else{
                throw new RuntimeException(e);
            }
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }



    public static void close(){
        LUA_THREAD.shutdown();
    }
}
