package com.verr1.controlcraft.unstable.blocks.autocannon;

import com.simibubi.create.foundation.block.IBE;
import com.verr1.controlcraft.foundation.api.common.ISignalAcceptor;
import com.verr1.controlcraft.foundation.api.common.ISignalHandler;
import com.verr1.controlcraft.foundation.redstone.IReceiver;
import com.verr1.controlcraft.registry.AIBlockEntities;
import com.verr1.controlcraft.unstable.ai.ui.AIUIFactory;
import com.verr1.controlcraft.unstable.blocks.AiCannonBaseBlockEntity;
import com.verr1.controlcraft.unstable.blocks.cruiser.CruiserBlockEntity;
import net.createmod.catnip.gui.ScreenOpener;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.LevelReader;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.DirectionalBlock;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.jetbrains.annotations.NotNull;

public class AiAutoCannonBlock extends DirectionalBlock implements IBE<AiAutocannonBlockEntity>, ISignalHandler {
    public static final String ID = "ai_autocannon";

    public AiAutoCannonBlock(Properties p_49795_) {
        super(p_49795_.noLootTable());
    }

    @Override
    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        super.createBlockStateDefinition(builder);
        builder.add(FACING);
    }

    @Override
    public Class<AiAutocannonBlockEntity> getBlockEntityClass() {
        return AiAutocannonBlockEntity.class;
    }

    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p){
        ScreenOpener.open(AIUIFactory.createAutoCannonScreen(p));
    }


    @Override
    public void neighborChanged(BlockState state, Level worldIn, BlockPos pos, Block blockIn, BlockPos fromPos,
                                boolean isMoving)  {
        ISignalHandler.super.onNeighborChanged(state, worldIn, pos, blockIn, fromPos, isMoving);
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

    @Override
    public BlockState getStateForPlacement(BlockPlaceContext context) {
        return defaultBlockState().setValue(FACING, context.getClickedFace());
    }

    @Override
    public BlockEntityType<? extends AiAutocannonBlockEntity> getBlockEntityType() {
        return AIBlockEntities.AI_AUTOCANNON_BLOCKENTITY.get();
    }

    @Override
    public void accept(Level level, BlockState state, BlockPos pos, Direction direction, int strength) {
        withBlockEntityDo(level, pos, be -> be.onRedstoneUpdate(strength));
    }
}
