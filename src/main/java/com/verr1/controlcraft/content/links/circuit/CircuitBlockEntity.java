package com.verr1.controlcraft.content.links.circuit;

import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import com.verr1.controlcraft.foundation.cimulink.game.port.packaged.CircuitLinkPort;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

public class CircuitBlockEntity extends CimulinkBlockEntity<CircuitLinkPort> {

    public static final NetworkKey CIRCUIT = NetworkKey.create("circuit");

    public CircuitBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
        buildRegistry(CIRCUIT)
                .withBasic(SerializePort.of(
                        () -> linkPort().serialize(),
                        t -> linkPort().deserialize(t)
                ))
                .register();
    }

    @Override
    protected CircuitLinkPort create() {
        return new CircuitLinkPort();
    }

    public void loadCircuit(CircuitNbt nbt){
        linkPort().load(nbt);
    }

}
