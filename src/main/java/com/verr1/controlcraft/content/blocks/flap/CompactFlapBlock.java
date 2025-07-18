package com.verr1.controlcraft.content.blocks.flap;

import com.simibubi.create.content.contraptions.bearing.BearingBlock;
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

public class CompactFlapBlock extends BearingBlock implements
        ISignalAcceptor, IBE<CompactFlapBlockEntity>
{

    public static final String ID = "compact_flap";

    public CompactFlapBlock(Properties p_52591_) {
        super(p_52591_);
    }


    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p){
        ScreenOpener.open(GenericUIFactory.createCompactFlapScreen(p));
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

        if(hit.getDirection() == state.getValue(FlapBearingBlock.FACING)){
            if (!player.mayBuild())
                return InteractionResult.FAIL;
            if (player.isShiftKeyDown())
                return InteractionResult.FAIL;
            if (player.getItemInHand(handIn)
                    .isEmpty()) {
                if (worldIn.isClientSide)
                    return InteractionResult.SUCCESS;
                withBlockEntityDo(worldIn, pos, be -> {
                    if(!be.running){
                        be.assemble();
                    }else{
                        be.disassemble();
                    }

                });
                return InteractionResult.SUCCESS;
            }
            return InteractionResult.PASS;
        }

        return InteractionResult.PASS;
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
