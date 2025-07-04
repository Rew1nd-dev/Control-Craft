package com.verr1.controlcraft.content.links.scope;

import com.simibubi.create.content.kinetics.base.IRotate;
import com.simibubi.create.foundation.gui.ScreenOpener;
import com.simibubi.create.foundation.utility.Iterate;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.gui.screens.OscilloscopeScreen;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.registry.CimulinkBlockEntities;
import com.verr1.controlcraft.registry.ControlCraftShapes;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.shapes.CollisionContext;
import net.minecraft.world.phys.shapes.Shapes;
import net.minecraft.world.phys.shapes.VoxelShape;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.jetbrains.annotations.NotNull;

public class OscilloscopeBlock extends CimulinkBlock<OscilloscopeBlockEntity> {
    public static final String ID = "scope";

    public OscilloscopeBlock(Properties p) {
        super(p);
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p) {
        ScreenOpener.open(new OscilloscopeScreen(p));
    }


    @Override
    public Class<OscilloscopeBlockEntity> getBlockEntityClass() {
        return OscilloscopeBlockEntity.class;
    }

    @Override
    public @NotNull VoxelShape getShape(BlockState state, BlockGetter worldIn, BlockPos pos, CollisionContext context) {
        return ControlCraftShapes.SCOPE_SHAPE_FACE.get(state.getValue(FACING).getOpposite());
    }



    @Override
    public BlockState getStateForPlacement(BlockPlaceContext context) {
        Direction nearestLookingDirection = context.getNearestLookingDirection();
        return defaultBlockState().setValue(FACING, nearestLookingDirection);
    }

    @Override
    public BlockEntityType<? extends OscilloscopeBlockEntity> getBlockEntityType() {
        return CimulinkBlockEntities.SCOPE_BLOCKENTITY.get();
    }
}
