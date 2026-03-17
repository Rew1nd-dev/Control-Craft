package com.verr1.controlcraft.content.links.computer;

import com.simibubi.create.foundation.block.IBE;
import com.verr1.controlcraft.registry.ControlCraftBlockEntities;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.DirectionalBlock;
import net.minecraft.world.level.block.HorizontalDirectionalBlock;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;

public class ComputerBlock extends HorizontalDirectionalBlock implements IBE<ComputerBlockEntity> {

    public static final String ID = "computer";

    public ComputerBlock(Properties p_52591_) {
        super(p_52591_);
    }

    @Override
    public void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        builder.add(FACING);
        super.createBlockStateDefinition(builder);
    }

    @Override
    public BlockState getStateForPlacement(BlockPlaceContext context) {
        return defaultBlockState().setValue(FACING, context.getHorizontalDirection().getOpposite());
    }

    @Override
    public Class<ComputerBlockEntity> getBlockEntityClass() {
        return ComputerBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends ComputerBlockEntity> getBlockEntityType() {
        return ControlCraftBlockEntities.COMPUTER_BLOCKENTITY.get();
    }
}
