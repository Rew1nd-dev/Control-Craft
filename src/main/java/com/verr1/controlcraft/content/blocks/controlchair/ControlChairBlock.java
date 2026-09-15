package com.verr1.controlcraft.content.blocks.controlchair;

import com.simibubi.create.content.contraptions.actors.seat.SeatBlock;
import com.simibubi.create.foundation.block.IBE;
import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.gui.factory.GenericUIFactory;
import com.verr1.controlcraft.registry.ControlCraftBlockEntities;
import net.minecraft.MethodsReturnNonnullByDefault;
import net.minecraft.core.BlockPos;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.DirectionalBlock;
import net.minecraft.world.level.block.HorizontalDirectionalBlock;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import javax.annotation.ParametersAreNonnullByDefault;

@ParametersAreNonnullByDefault
@MethodsReturnNonnullByDefault
public class ControlChairBlock extends SeatBlock implements IBE<ControlChairBlockEntity> {
    public static final String ID = "control_chair";

    public ControlChairBlock(Properties properties) {
        super(properties, DyeColor.GRAY);
    }

    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos pos) {
        ScreenOpener.open(GenericUIFactory.createControlChairScreen(pos));
    }

    @Override
    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        builder.add(HorizontalDirectionalBlock.FACING);
        super.createBlockStateDefinition(builder);
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
                && player.isShiftKeyDown()
                && player.getItemInHand(InteractionHand.MAIN_HAND).isEmpty()) {
            if (level.isClientSide) {
                displayScreen(pos);
            }
            return InteractionResult.SUCCESS;
        }

        return super.use(state, level, pos, player, hand, hit);
    }

    @Override
    public Class<ControlChairBlockEntity> getBlockEntityClass() {
        return ControlChairBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends ControlChairBlockEntity> getBlockEntityType() {
        return ControlCraftBlockEntities.CONTROL_CHAIR_BLOCKENTITY.get();
    }
}
