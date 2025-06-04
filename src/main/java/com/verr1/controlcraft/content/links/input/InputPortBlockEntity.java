package com.verr1.controlcraft.content.links.input;

import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.port.inout.InputLinkPort;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.NotNull;

public class InputPortBlockEntity extends CimulinkBlockEntity<InputLinkPort> {
    public static final NetworkKey INPUT = NetworkKey.create("link_input");

    public InputPortBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
        buildRegistry(INPUT)
                .withBasic(SerializePort.of(
                        () -> linkPort().peek(),
                        t -> linkPort().input(t),
                        SerializeUtils.DOUBLE
                ))
                .withClient(ClientBuffer.DOUBLE.get())
                .register();
    }

    @Override
    public void tickServer() {
        super.tickServer();
        linkPort().tick();
    }

    @Override
    protected InputLinkPort create(@NotNull Level level, BlockPos pos) {
        return new InputLinkPort(WorldBlockPos.of(level, pos));
    }
}
