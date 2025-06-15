package com.verr1.controlcraft.content.links.func;

import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.port.analog.FunctionsLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.AnalogTypes;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.FFTypes;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

public class FunctionsBlockEntity extends CimulinkBlockEntity<FunctionsLinkPort> {

    public static final NetworkKey FUNCTIONS = NetworkKey.create("functions_type");

    public FunctionsBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
        buildRegistry(FUNCTIONS)
                .withBasic(SerializePort.of(
                        () -> linkPort().getCurrentType(),
                        t ->  linkPort().setCurrentType(t),
                        SerializeUtils.ofEnum(AnalogTypes.class)
                ))
                .withClient(ClientBuffer.of(AnalogTypes.class))
                .runtimeOnly() // types will be saved by port itself
                .register();
    }

    @Override
    protected FunctionsLinkPort create() {
        return new FunctionsLinkPort();
    }
}
