package com.verr1.controlcraft.foundation.cimulink.core.components.luacuit;

import com.verr1.controlcraft.foundation.cimulink.core.components.lua.CimulinkLua;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import net.minecraft.nbt.CompoundTag;

import java.util.List;

public record LuacuitScript(String code, List<String> definedInputs, List<String> definedOutputs) {
    public static final Serializer<List<String>> STRING_LIST_SER = SerializeUtils.ofList(SerializeUtils.STRING);
    public static final LuacuitScript EMPTY = new LuacuitScript(CimulinkLua.EMPTY_CODE, List.of(), List.of());

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

}
