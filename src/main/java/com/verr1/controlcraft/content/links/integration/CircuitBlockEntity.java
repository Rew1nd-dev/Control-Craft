package com.verr1.controlcraft.content.links.integration;

import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.Circuit;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import com.verr1.controlcraft.foundation.cimulink.game.port.packaged.CircuitLinkPort;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

import static java.lang.Math.min;

public class CircuitBlockEntity extends WirelessIntegrationBlockEntity<Circuit, CircuitLinkPort> {


    public CircuitBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);

    }


    @Override
    protected CircuitLinkPort create() {
        return new CircuitLinkPort();
    }

    public void loadCircuit(CircuitNbt nbt) throws IllegalArgumentException{
        var savedStatus = linkPort().viewStatus();
        boolean shouldOpen = linkPort().isEmpty();
        linkPort().load(nbt);
        linkPort().setStatus(savedStatus);
        if(shouldOpen)linkPort().setToAllOpen();
        updateIOName();
        setChanged();
    }


}
