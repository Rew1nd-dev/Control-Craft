package com.verr1.controlcraft.content.links.logic;

import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.port.digital.GateLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.GateTypes;
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

public class LogicGateBlockEntity extends CimulinkBlockEntity<GateLinkPort> {

    public static final NetworkKey GATE_TYPE = NetworkKey.create("gate_type");

    @Override
    protected GateLinkPort create(@NotNull Level level, BlockPos pos) {
        return new GateLinkPort(WorldBlockPos.of(level, getBlockPos()));
    }

    public LogicGateBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);

        buildRegistry(GATE_TYPE)
            .withBasic(SerializePort.of(
                () -> linkPort().getCurrentType(),
                t ->  linkPort().setCurrentType(t),
                SerializeUtils.ofEnum(GateTypes.class)
            ))
            .withClient(ClientBuffer.of(GateTypes.class))
            .runtimeOnly()
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


}
