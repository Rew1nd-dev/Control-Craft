package com.verr1.controlcraft.foundation.cimulink.game.port.packaged;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.links.integration.CircuitBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.core.api.IBusAccess;
import com.verr1.controlcraft.foundation.cimulink.core.api.IPhysAccess;
import com.verr1.controlcraft.foundation.cimulink.core.api.IWorldAccess;
import com.verr1.controlcraft.foundation.cimulink.core.components.lua.CimulinkLua;
import com.verr1.controlcraft.foundation.cimulink.core.components.luacuit.Luacuit;
import com.verr1.controlcraft.foundation.cimulink.core.components.luacuit.LuacuitConstructor;
import com.verr1.controlcraft.foundation.cimulink.core.components.luacuit.LuacuitScript;
import com.verr1.controlcraft.foundation.cimulink.core.registry.CimulinkFactory;
import com.verr1.controlcraft.foundation.cimulink.core.registry.Factory;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.LuaOvertimeException;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.UndefineMethodException;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import net.minecraft.nbt.CompoundTag;
import org.jetbrains.annotations.Nullable;
import org.luaj.vm2.LuaError;

import java.util.List;

public class LuacuitLinkPort extends WrappedLinkPort<Luacuit> {

    private LuacuitScript script = LuacuitScript.EMPTY;

    public LuacuitLinkPort() {
        super(CimulinkLua.EMPTY_LUACUIT);
    }

    @Override
    public Luacuit component() {
        return (Luacuit) (proxy().plant());
    }

    @Override
    public Factory<Luacuit> factory() {
        return CimulinkFactory.LUACUIT;
    }

    public void load(@Nullable LuacuitScript script) throws IllegalArgumentException {
        this.script = script == null || script.code().isEmpty() ? LuacuitScript.EMPTY : script;
        buildCached();
        recreate();
    }

    private void buildCached() {
        try {
            cached = new LuacuitConstructor(script).build();
            cachedEnabledInputs = cached.inputs();
            cachedEnabledOutputs = cached.outputs();
        } catch (UndefineMethodException | LuaError | LuaOvertimeException e) {
            cached = CimulinkLua.EMPTY_LUACUIT;
            cachedEnabledInputs = List.of();
            cachedEnabledOutputs = List.of();
            ControlCraft.LOGGER.error("build new luacuit failed: {}", e.getMessage());
        }
    }

    public boolean isEmpty() {
        return script == LuacuitScript.EMPTY;
    }

    public void setPhysAccess(IPhysAccess access) {
        component().setPhysAccess(access);
    }

    public void setWorldAccess(IWorldAccess access) {
        component().setUtilAccess(access);
    }

    public void setBusAccess(IBusAccess access) {
        component().setBusAccess(access);
    }

    @Override
    public CompoundTag serialize() {
        return CompoundTagBuilder.create()
                .withCompound("blp", super.serialize())
                .withCompound("lua", script.serialize())
                .withCompound("status", CircuitBlockEntity.PAIR_SER.serialize(viewStatus()))
                .build();
    }

    @Override
    public void deserialize(CompoundTag tag) {
        load(LuacuitScript.deserialize(tag.getCompound("lua")));
        if (tag.contains("status"))
            setStatus(CircuitBlockEntity.PAIR_SER.deserialize(tag.getCompound("status")));

        super.deserialize(tag.getCompound("blp"));
    }

}
