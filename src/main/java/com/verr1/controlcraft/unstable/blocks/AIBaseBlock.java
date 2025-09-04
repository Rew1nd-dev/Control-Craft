package com.verr1.controlcraft.unstable.blocks;

import com.simibubi.create.foundation.block.IBE;
import com.verr1.controlcraft.foundation.api.common.ISignalHandler;
import net.minecraft.core.BlockPos;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.DirectionalBlock;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.jetbrains.annotations.NotNull;

public abstract class AIBaseBlock<T extends BlockEntity> extends DirectionalBlock implements IBE<T> {
    protected AIBaseBlock(Properties p_52591_) {
        super(p_52591_.strength(-1, 3600000.0f));
    }

    @Override
    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        super.createBlockStateDefinition(builder);
        builder.add(FACING);
    }
    @OnlyIn(Dist.CLIENT)
    public abstract void displayScreen(BlockPos p);

    @Override
    public void neighborChanged(@NotNull BlockState state, @NotNull Level worldIn, @NotNull BlockPos pos, @NotNull Block blockIn, @NotNull BlockPos fromPos,
                                boolean isMoving)  {
        if(this instanceof ISignalHandler handler){
            handler.onNeighborChanged(state, worldIn, pos, blockIn, fromPos, isMoving);
        }
    }

    @Override
    public @NotNull InteractionResult use(BlockState state, Level worldIn, BlockPos pos, Player player, InteractionHand handIn,
                                          BlockHitResult hit){
        if(     worldIn.isClientSide
                && handIn == InteractionHand.MAIN_HAND
                && player.getItemInHand(InteractionHand.MAIN_HAND).isEmpty()
                && !player.isShiftKeyDown()
        ){
            displayScreen(pos);
            return InteractionResult.SUCCESS;
        }
        return InteractionResult.PASS;
    }

}
