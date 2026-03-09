package com.verr1.controlcraft.content.links.screen_base;

import com.simibubi.create.foundation.block.IBE;
import com.verr1.controlcraft.registry.ControlCraftBlockEntities;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.DirectionalBlock;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;

public class ComputerBaseBlock extends DirectionalBlock implements IBE<ComputerBaseBlockEntity> {

    public static final String ID = "computer";

    public ComputerBaseBlock(Properties p_52591_) {
        super(p_52591_);
    }

    @Override
    public void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        builder.add(FACING);
        super.createBlockStateDefinition(builder);
    }

    @Override
    public BlockState getStateForPlacement(BlockPlaceContext context) {
        return defaultBlockState().setValue(FACING, context.getClickedFace());
    }

    @Override
    public Class<ComputerBaseBlockEntity> getBlockEntityClass() {
        return ComputerBaseBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends ComputerBaseBlockEntity> getBlockEntityType() {
        return ControlCraftBlockEntities.COMPUTER_BLOCKENTITY.get();
    }
}
