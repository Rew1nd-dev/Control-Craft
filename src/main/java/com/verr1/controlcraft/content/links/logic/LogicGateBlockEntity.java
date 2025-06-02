package com.verr1.controlcraft.content.links.logic;

import com.verr1.controlcraft.content.blocks.NetworkBlockEntity;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

public class LogicGateBlockEntity extends NetworkBlockEntity {

    public LogicGateBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
    }
}
