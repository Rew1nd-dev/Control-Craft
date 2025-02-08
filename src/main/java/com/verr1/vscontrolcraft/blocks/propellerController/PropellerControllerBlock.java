package com.verr1.vscontrolcraft.blocks.propellerController;

import com.simibubi.create.content.kinetics.base.DirectionalKineticBlock;
import com.simibubi.create.content.kinetics.simpleRelays.ICogWheel;
import com.simibubi.create.foundation.block.IBE;
import com.verr1.vscontrolcraft.registry.AllBlockEntities;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.LevelReader;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.DirectionalBlock;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import org.jetbrains.annotations.Nullable;

public class PropellerControllerBlock extends DirectionalKineticBlock implements IBE<PropellerControllerBlockEntity> {

    public static final String ID = "propeller_controller";

    public PropellerControllerBlock(Properties properties) {
        super(properties);
    }


    @Nullable
    @Override
    public BlockState getStateForPlacement(BlockPlaceContext context) {

        return defaultBlockState().setValue(FACING, context.getClickedFace().getOpposite());
    }

    @Override
    public Class<PropellerControllerBlockEntity> getBlockEntityClass() {
        return PropellerControllerBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends PropellerControllerBlockEntity> getBlockEntityType() {
        return AllBlockEntities.PROPELLER_CONTROLLER_BLOCKENTITY.get();
    }

    @Override
    public Direction.Axis getRotationAxis(BlockState state) {
        return state.getValue(FACING).getAxis();
    }

    @Override
    public boolean hasShaftTowards(LevelReader world, BlockPos pos, BlockState state, Direction face) {
        return face == state.getValue(FACING).getOpposite();
    }
}
