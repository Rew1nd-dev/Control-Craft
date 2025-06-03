package com.verr1.controlcraft.content.links.logic;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.blocks.NetworkBlockEntity;
import com.verr1.controlcraft.content.blocks.SharedKeys;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.ILinkableBlock;
import com.verr1.controlcraft.foundation.cimulink.game.port.digital.LogicGateLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.GateTypes;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.CompoundTagPort;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

public class LogicGateBlockEntity extends NetworkBlockEntity implements ILinkableBlock {


    private final LogicGateLinkPort linkPort;

    public static final NetworkKey GATE_TYPE = NetworkKey.create("gate_type");




    public LogicGateBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
        if(level == null){
            ControlCraft.LOGGER.error("LogicGate Be Created With Null Level! at: {}", pos);
        }
        linkPort = new LogicGateLinkPort(WorldBlockPos.of(level, pos));


        buildRegistry(GATE_TYPE)
            .withBasic(SerializePort.of(
                linkPort::getCurrentType,
                linkPort::setCurrentType,
                SerializeUtils.ofEnum(GateTypes.class)
            ))
            .withClient(ClientBuffer.of(GateTypes.class))
            .runtimeOnly()
            .register();

        buildRegistry(SharedKeys.BLP)
            .withBasic(CompoundTagPort.of(
                linkPort::serialize,
                linkPort::deserialize
            ))
            .dispatchToSync()
            .register();

    }

    @Override
    public void tickServer() {
        super.tickServer();
        syncForNear(false, GATE_TYPE);
    }


    public GateTypes readClientGateType(){
        return handler().readClientBuffer(GATE_TYPE, GateTypes.class);
    }

    @Override
    public BlockLinkPort linkPort() {
        return linkPort;
    }
}
