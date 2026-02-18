package com.verr1.controlcraft.foundation.cimulink.core.components.luacuit;

import com.verr1.controlcraft.foundation.cimulink.core.api.IBusAccess;
import com.verr1.controlcraft.foundation.cimulink.core.api.IPhysAccess;
import com.verr1.controlcraft.foundation.cimulink.core.api.IWorldAccess;
import com.verr1.controlcraft.foundation.cimulink.core.components.lua.BusLib;
import com.verr1.controlcraft.foundation.cimulink.core.components.lua.CimulinkLua;
import com.verr1.controlcraft.foundation.cimulink.core.components.lua.PhysLib;
import com.verr1.controlcraft.foundation.cimulink.core.components.lua.UtilLib;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.LuaOvertimeException;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import kotlin.Pair;
import net.minecraft.nbt.CompoundTag;
import org.luaj.vm2.Globals;
import org.luaj.vm2.LuaError;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.lib.OneArgFunction;
import org.luaj.vm2.lib.TwoArgFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.*;

public record LuacuitScript(String code, List<String> definedInputs, List<Double> defaultInputs, List<String> definedOutputs) {
    public static final Serializer<List<Double>> DOUBLE_LIST_SER = SerializeUtils.ofList(SerializeUtils.DOUBLE);
    public static final Serializer<List<String>> STRING_LIST_SER = SerializeUtils.ofList(SerializeUtils.STRING);
    public static final String EMPTY_CODE =
            """
            
            function define()
            
            end
            
            function loop()
            
            end
            
            """;
    public static final LuacuitScript EMPTY = new LuacuitScript(EMPTY_CODE, List.of(), List.of(), List.of());

    public CompoundTag serialize(){
        return CompoundTagBuilder.create()
                .withString("code", code)
                .withCompound("inputs", STRING_LIST_SER.serialize(definedInputs))
                .withCompound("default", DOUBLE_LIST_SER.serialize(defaultInputs))
                .withCompound("outputs", STRING_LIST_SER.serialize(definedOutputs))
                .build();
    }

    public static LuacuitScript deserialize(CompoundTag tag){
        return new LuacuitScript(
                tag.getString("code"),
                STRING_LIST_SER.deserialize(tag.getCompound("inputs")),
                DOUBLE_LIST_SER.deserialize(tag.getCompound("default")),
                STRING_LIST_SER.deserialize(tag.getCompound("outputs"))
        );
    }

    public static LuacuitScript fromCode(String code) throws LuaOvertimeException, LuaError {
        LuacuitScript temporary;
        Globals defineGlobal = CimulinkLua.createStandardGlobals();
        defineGlobal.load(new PhysLib(IPhysAccess.EMPTY));
        defineGlobal.load(new UtilLib(IWorldAccess.EMPTY));
        defineGlobal.load(new BusLib(IBusAccess.EMPTY));

        List<String> definedInputs = new ArrayList<>();
        List<Double> defaultInputs = new ArrayList<>();
        List<String> collectedOutputs = new ArrayList<>();

        defineGlobal.set("defineInput", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue arg, LuaValue arg1) {
                String name = arg.checkjstring();
                double defaultVal = arg1 == LuaValue.NIL ? 0.0 : arg1.checkdouble();
                definedInputs.add(name);
                defaultInputs.add(defaultVal);
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
                definedInputs,
                defaultInputs,
                collectedOutputs
        );
        return temporary;
    }


    public double getDefault(String input){
        int idx = definedInputs.indexOf(input);
        if(definedInputs.size() != defaultInputs.size() || (idx < 0 || idx > defaultInputs.size())){
            return 0.0;
        }
        return defaultInputs.get(idx);
    }

}
