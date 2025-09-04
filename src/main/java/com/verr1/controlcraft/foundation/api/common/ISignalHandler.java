package com.verr1.controlcraft.foundation.api.common;

import com.verr1.controlcraft.foundation.redstone.IReceiver;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;

public interface ISignalHandler extends ISignalAcceptor{

    void accept(Level level, BlockState state, BlockPos pos, Direction direction, int strength);

    @Override
    default void onNeighborChanged(BlockState state, Level worldIn, BlockPos pos, Block blockIn, BlockPos fromPos, boolean isMoving) {
        if(worldIn.isClientSide)return;
        Direction direction = Direction.fromDelta(
                fromPos.getX() - pos.getX(),
                fromPos.getY() - pos.getY(),
                fromPos.getZ() - pos.getZ()
        );
        if(direction == null)return;
        accept(worldIn, state, pos, direction, worldIn.getSignal(fromPos, direction));
    }
}
