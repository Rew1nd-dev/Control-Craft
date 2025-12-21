package com.verr1.controlcraft.foundation.cimulink.game.port.packaged;

import com.verr1.controlcraft.content.links.integration.CircuitBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.Circuit;
import com.verr1.controlcraft.foundation.cimulink.core.registry.CimulinkFactory;
import com.verr1.controlcraft.foundation.cimulink.core.registry.Factory;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import net.minecraft.nbt.CompoundTag;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;


public class CircuitLinkPort extends WrappedLinkPort<Circuit> {



    private @NotNull CircuitNbt nbt = CircuitNbt.EMPTY_CONTEXT;


    public CircuitLinkPort() {
        super(CircuitNbt.EMPTY_CIRCUIT);
    }


    public void load(@Nullable CircuitNbt nbt) throws IllegalArgumentException{
        this.nbt = nbt == null ? CircuitNbt.EMPTY_CONTEXT : nbt;
        buildCached();
        recreate();
    }

    private void buildCached(){
        try{
            cached = nbt.buildCircuit();
            cachedEnabledInputs = cached.inputNamesValid();
            cachedEnabledOutputs = cached.outputs();
        }catch (IllegalArgumentException e){
            throw new IllegalArgumentException("build cached circuit failed: " + e.getMessage() + " suspected: " + nbt);
        }

    }

    public boolean isEmpty(){
        return nbt == CircuitNbt.EMPTY_CONTEXT;
    }

    @Override
    protected List<String> inputNamesValid() {
        return component().inputNamesValid();
    }

    public Circuit component(){
        return (Circuit)(proxy().plant());
    }


    @Override
    public Factory<Circuit> factory() {
        return CimulinkFactory.CIRCUIT;
    }

    @Override
    public CompoundTag serialize() {
        return CompoundTagBuilder.create()
                .withCompound("blp", super.serialize())
                .withCompound("circuit", nbt.serialize())
                .withCompound("status", CircuitBlockEntity.PAIR_SER.serialize(viewStatus()))
                .build();
    }

    @Override
    public void deserialize(CompoundTag tag) {
        load(CircuitNbt.deserialize(tag.getCompound("circuit")));
        if(tag.contains("status"))setStatus(CircuitBlockEntity.PAIR_SER.deserialize(tag.getCompound("status")));

        super.deserialize(tag.getCompound("blp"));
    }
}
