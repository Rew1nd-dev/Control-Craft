package com.verr1.controlcraft.unstable.blocks.cannon;

import com.simibubi.create.foundation.block.IBE;
import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.foundation.api.common.ISignalHandler;
import com.verr1.controlcraft.registry.AIBlockEntities;
import com.verr1.controlcraft.unstable.ai.ui.AIUIFactory;
import com.verr1.controlcraft.unstable.blocks.cruiser.CruiserBlockEntity;
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
import org.jetbrains.annotations.NotNull;

public class AiBigCannonBlock extends DirectionalBlock implements IBE<AiBigCannonBlockEntity>, ISignalHandler {
    public static final String ID = "ai_big_cannon";

    public AiBigCannonBlock(Properties p_49795_) {
        super(p_49795_.noLootTable());
    }

    @Override
    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        super.createBlockStateDefinition(builder);
        builder.add(FACING);
    }

    @Override
    public void neighborChanged(BlockState state, Level worldIn, BlockPos pos, Block blockIn, BlockPos fromPos,
                                boolean isMoving)  {
        ISignalHandler.super.onNeighborChanged(state, worldIn, pos, blockIn, fromPos, isMoving);
    }

    @Override
    public Class<AiBigCannonBlockEntity> getBlockEntityClass() {
        return AiBigCannonBlockEntity.class;
    }

    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p){
        ScreenOpener.open(AIUIFactory.createAutoCannonScreen(p));
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
    public BlockEntityType<? extends AiBigCannonBlockEntity> getBlockEntityType() {
        return AIBlockEntities.AI_BIG_CANNON_BLOCKENTITY.get();
    }

    @Override
    public void accept(Level level, BlockState state, BlockPos pos, Direction direction, int strength) {
        withBlockEntityDo(level, pos, be -> be.onRedstoneUpdate(strength));
    }
}
