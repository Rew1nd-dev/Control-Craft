package com.verr1.controlcraft.content.links.logic;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.blocks.NetworkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.ILinkableBlock;
import com.verr1.controlcraft.foundation.cimulink.game.port.digital.LogicGateLinkPort;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

public class LogicGateBlockEntity extends NetworkBlockEntity implements ILinkableBlock {


    private final LogicGateLinkPort linkPort;

    public LogicGateBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
        if(level == null){
            ControlCraft.LOGGER.error("LogicGate Be Created With Null Level! at: {}", pos);
        }
        linkPort = new LogicGateLinkPort(WorldBlockPos.of(level, pos));
    }

    @Override
    public BlockLinkPort linkPort() {
        return linkPort;
    }
}
