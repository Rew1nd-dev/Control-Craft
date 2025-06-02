package com.verr1.controlcraft.content.links.logic;

import com.simibubi.create.foundation.block.IBE;
import com.verr1.controlcraft.registry.CimulinkBlockEntity;
import net.minecraft.core.BlockPos;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.DirectionalBlock;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.phys.shapes.CollisionContext;
import net.minecraft.world.phys.shapes.VoxelShape;

import static com.verr1.controlcraft.registry.ControlCraftShapes.HALF_BOX_BASE;

public class LogicGateBlock extends DirectionalBlock implements IBE<LogicGateBlockEntity> {
    public static final String ID = "logic_gates";


    public LogicGateBlock(Properties properties) {
        super(properties);
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
    public VoxelShape getShape(BlockState state, BlockGetter worldIn, BlockPos pos, CollisionContext context) {
        return HALF_BOX_BASE.get(state.getValue(FACING));
    }

    @Override
    public Class<LogicGateBlockEntity> getBlockEntityClass() {
        return LogicGateBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends LogicGateBlockEntity> getBlockEntityType() {
        return CimulinkBlockEntity.LOGIC_GATE_BLOCKENTITY.get();
    }
}
