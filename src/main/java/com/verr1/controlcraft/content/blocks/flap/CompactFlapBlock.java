package com.verr1.controlcraft.content.blocks.flap;

import com.simibubi.create.foundation.block.IBE;
import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.blocks.jet.JetBlockEntity;
import com.verr1.controlcraft.content.gui.factory.GenericUIFactory;
import com.verr1.controlcraft.foundation.api.common.ISignalAcceptor;
import com.verr1.controlcraft.registry.ControlCraftBlockEntities;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.DirectionalBlock;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.jetbrains.annotations.Nullable;

public class CompactFlapBlock extends DirectionalBlock implements
        ISignalAcceptor, IBE<CompactFlapBlockEntity>
{

    public static final String ID = "compact_flap";

    public CompactFlapBlock(Properties p_52591_) {
        super(p_52591_);
    }


    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p){
        // ScreenOpener.open(GenericUIFactory.createJetScreen(p));
    }


    @Override
    public void neighborChanged(BlockState state, Level worldIn, BlockPos pos, Block blockIn, BlockPos fromPos,
                                boolean isMoving)  {
        ISignalAcceptor.super.onNeighborChanged(state, worldIn, pos, blockIn, fromPos, isMoving);
    }


    @Override
    public InteractionResult use(BlockState state, Level worldIn, BlockPos pos, Player player, InteractionHand handIn,
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

    @Override
    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        builder.add(FACING);
        super.createBlockStateDefinition(builder);
    }

    @Nullable
    @Override
    public BlockState getStateForPlacement(BlockPlaceContext context) {
        Direction face = context.getClickedFace();
        if(context.getPlayer() != null && context.getPlayer().isShiftKeyDown())face = face.getOpposite();
        return defaultBlockState().setValue(FACING, face);
    }

    @Override
    public Class<CompactFlapBlockEntity> getBlockEntityClass() {
        return CompactFlapBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends CompactFlapBlockEntity> getBlockEntityType() {
        return ControlCraftBlockEntities.COMPACT_FLAP_BLOCKENTITY.get();
    }

}
