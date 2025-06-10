package com.verr1.controlcraft.content.links.shifter;

import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.port.digital.ShifterLinkPort;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

public class ShifterLinkBlockEntity extends CimulinkBlockEntity<ShifterLinkPort> {
    public static final NetworkKey PARALLEL = NetworkKey.create("parallel");
    public static final NetworkKey DELAY = NetworkKey.create("delay");
    public static final NetworkKey ASYNC = NetworkKey.create("async_shifter");


    public ShifterLinkBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
        buildRegistry(PARALLEL)
                .withBasic(SerializePort.of(
                        () -> linkPort().parallel(),
                        p -> linkPort().setParallel(p),
                        SerializeUtils.LONG
                ))
                .withClient(ClientBuffer.LONG.get())
                .register();

        buildRegistry(DELAY)
                .withBasic(SerializePort.of(
                        () -> linkPort().delay(),
                        p -> linkPort().setDelay(p),
                        SerializeUtils.LONG
                ))
                .withClient(ClientBuffer.LONG.get())
                .register();

        buildRegistry(ASYNC)
                .withBasic(SerializePort.of(
                        () -> linkPort().async(),
                        a -> linkPort().setAsync(a),
                        SerializeUtils.BOOLEAN
                ))
                .withClient(ClientBuffer.BOOLEAN.get())
                .register();

    }

    @Override
    protected ShifterLinkPort create() {
        return new ShifterLinkPort();
    }
}
