package com.verr1.controlcraft.foundation.cimulink.core.components.luacuit;

import com.verr1.controlcraft.foundation.cimulink.core.components.lua.CimulinkLua;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.LuaOvertimeException;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import net.minecraft.nbt.CompoundTag;
import org.luaj.vm2.Globals;
import org.luaj.vm2.LuaError;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.lib.OneArgFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.*;

public record LuacuitScript(String code, List<String> definedInputs, List<String> definedOutputs) {
    public static final Serializer<List<String>> STRING_LIST_SER = SerializeUtils.ofList(SerializeUtils.STRING);
    public static final String EMPTY_CODE =
            """
            
            function define()
            
            end
            
            function loop()
            
            end
            
            """;
    public static final LuacuitScript EMPTY = new LuacuitScript(EMPTY_CODE, List.of(), List.of());

    public CompoundTag serialize(){
        return CompoundTagBuilder.create()
                .withString("code", code)
                .withCompound("inputs", STRING_LIST_SER.serialize(definedInputs))
                .withCompound("outputs", STRING_LIST_SER.serialize(definedOutputs))
                .build();
    }

    public static LuacuitScript deserialize(CompoundTag tag){
        return new LuacuitScript(
                tag.getString("code"),
                STRING_LIST_SER.deserialize(tag.getCompound("inputs")),
                STRING_LIST_SER.deserialize(tag.getCompound("outputs"))
        );
    }

    public static LuacuitScript fromCode(String code) throws LuaOvertimeException, LuaError {
        LuacuitScript temporary;
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
            //.completeOnTimeout(null, 30, TimeUnit.MILLISECONDS);;

        try{
            future.get(30, TimeUnit.MILLISECONDS);
        } catch (ExecutionException e) {
            if(e.getCause() instanceof LuaError luaError){
                throw luaError;
            }
            throw new RuntimeException(e);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        } catch (TimeoutException e) {
            throw new LuaOvertimeException("Lua script compilation timed out.");
        }


        temporary = new LuacuitScript(
                code,
                collectedInputs,
                collectedOutputs
        );
        return temporary;
    }

}
