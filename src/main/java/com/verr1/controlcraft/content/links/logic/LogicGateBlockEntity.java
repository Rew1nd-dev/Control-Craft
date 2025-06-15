package com.verr1.controlcraft.content.links.logic;

import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.content.links.ff.FFBlock;
import com.verr1.controlcraft.foundation.cimulink.game.port.digital.GateLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.FFTypes;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.GateTypes;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.utils.MinecraftUtils;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

public class LogicGateBlockEntity extends CimulinkBlockEntity<GateLinkPort> {

    public static final NetworkKey GATE_TYPE = NetworkKey.create("gate_type");

    @Override
    protected GateLinkPort create() {
        return new GateLinkPort();
    }

    public LogicGateBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);

        buildRegistry(GATE_TYPE)
            .withBasic(SerializePort.of(
                () -> linkPort().getCurrentType(),
                    this::setCurrentType,
                SerializeUtils.ofEnum(GateTypes.class)
            ))
            .withClient(ClientBuffer.of(GateTypes.class))
            .runtimeOnly()
            .register();

    }

    public void setCurrentType(GateTypes t){
        linkPort().setCurrentType(t);
        MinecraftUtils.updateBlockState(level, getBlockPos(), getBlockState().setValue(LogicGateBlock.TYPE, t));
    }

    @Override
    public void tickServer() {
        super.tickServer();
        syncForNear(false, GATE_TYPE);
    }

    public GateTypes readClientGateType(){
        return handler().readClientBuffer(GATE_TYPE, GateTypes.class);
    }


}
