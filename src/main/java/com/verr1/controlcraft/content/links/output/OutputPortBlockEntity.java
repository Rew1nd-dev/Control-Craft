package com.verr1.controlcraft.content.links.output;

import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.port.inout.OutputLinkPort;
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

public class OutputPortBlockEntity extends CimulinkBlockEntity<OutputLinkPort> {

    public static final NetworkKey OUTPUT = NetworkKey.create("link_output");

    public OutputPortBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
        buildRegistry(OUTPUT)
                .withBasic(SerializePort.of(
                        () -> linkPort().peek(),
                        $ -> {},
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
    protected OutputLinkPort create(@NotNull Level level, BlockPos pos) {
        return new OutputLinkPort(WorldBlockPos.of(level, pos));
    }
}
