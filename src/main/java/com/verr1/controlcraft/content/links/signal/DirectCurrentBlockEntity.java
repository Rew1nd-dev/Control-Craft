package com.verr1.controlcraft.content.links.signal;

import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.port.signal.DirectCurrentPort;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

public class DirectCurrentBlockEntity extends CimulinkBlockEntity<DirectCurrentPort> {

    public static final NetworkKey DC = NetworkKey.create("direct_current");

    public DirectCurrentBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);

        buildRegistry(DC)
                .withBasic(SerializePort.of(
                        () -> linkPort().getValue(),
                        t -> linkPort().setValue(t),
                        SerializeUtils.DOUBLE
                ))
                .runtimeOnly()
                .withClient(ClientBuffer.DOUBLE.get())
                .register();

    }

    @Override
    protected DirectCurrentPort create() {
        return new DirectCurrentPort();
    }
}
