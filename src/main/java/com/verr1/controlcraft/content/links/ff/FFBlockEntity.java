package com.verr1.controlcraft.content.links.ff;

import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.port.digital.FFLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.FFTypes;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

public class FFBlockEntity extends CimulinkBlockEntity<FFLinkPort> {

    public static final NetworkKey FF_TYPE = NetworkKey.create("gate_type");

    public FFBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
        buildRegistry(FF_TYPE)
                .withBasic(SerializePort.of(
                        () -> linkPort().getCurrentType(),
                        t ->  linkPort().setCurrentType(t),
                        SerializeUtils.ofEnum(FFTypes.class)
                ))
                .withClient(ClientBuffer.of(FFTypes.class))
                .runtimeOnly()
                .register();
    }

    @Override
    protected FFLinkPort create() {
        return new FFLinkPort();
    }
}
