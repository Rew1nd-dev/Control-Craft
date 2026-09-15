package com.verr1.controlcraft.content.links.computer;

import com.simibubi.create.foundation.block.IBE;
import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.gui.factory.GenericUIFactory;
import com.verr1.controlcraft.registry.ControlCraftBlockEntities;
import net.minecraft.MethodsReturnNonnullByDefault;
import net.minecraft.core.BlockPos;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.DirectionalBlock;
import net.minecraft.world.level.block.HorizontalDirectionalBlock;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import javax.annotation.ParametersAreNonnullByDefault;

@ParametersAreNonnullByDefault
@MethodsReturnNonnullByDefault
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

    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos pos) {
        ScreenOpener.open(GenericUIFactory.createComputerScreen(pos));
    }

    @Override
    public InteractionResult use(
            BlockState state,
            Level level,
            BlockPos pos,
            Player player,
            InteractionHand hand,
            BlockHitResult hit
    ) {
        if (hand == InteractionHand.MAIN_HAND
                && !player.isShiftKeyDown()
                && player.getItemInHand(InteractionHand.MAIN_HAND).isEmpty()) {
            if (level.isClientSide) {
                displayScreen(pos);
            }
            return InteractionResult.SUCCESS;
        }

        return InteractionResult.PASS;
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
