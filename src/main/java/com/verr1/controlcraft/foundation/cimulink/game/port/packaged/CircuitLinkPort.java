package com.verr1.controlcraft.foundation.cimulink.game.port.packaged;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.Circuit;
import com.verr1.controlcraft.foundation.cimulink.core.components.sources.Sink;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.Summary;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.ICompilable;
import com.verr1.controlcraft.foundation.cimulink.game.port.ISummarizable;
import com.verr1.controlcraft.foundation.cimulink.game.registry.CimulinkFactory;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import net.minecraft.nbt.CompoundTag;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;



public class CircuitLinkPort extends BlockLinkPort implements ICompilable<Circuit> {

    private @NotNull CircuitNbt nbt = CircuitNbt.EMPTY_CONTEXT;

    public CircuitLinkPort() {
        super(CircuitNbt.EMPTY_CIRCUIT);
    }


    public void load(@Nullable CircuitNbt nbt){
        this.nbt = nbt == null ? CircuitNbt.EMPTY_CONTEXT : nbt;
        recreate();
    }

    @Override
    public NamedComponent create() {
        return nbt.buildCircuit();
    }

    @Override
    public Circuit component() {
        return (Circuit) __raw();
    }

    @Override
    public CimulinkFactory.Factory<Circuit> factory() {
        return CimulinkFactory.CIRCUIT;
    }


    @Override
    public CompoundTag serialize() {
        return CompoundTagBuilder.create()
                .withCompound("blp", super.serialize())
                .withCompound("circuit", nbt.serialize())
                .build();
    }

    @Override
    public void deserialize(CompoundTag tag) {

        load(CircuitNbt.deserialize(tag.getCompound("circuit")));

        super.deserialize(tag.getCompound("blp"));
    }
}
