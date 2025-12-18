package com.verr1.controlcraft.unstable.blocks.cruiser;

import com.simibubi.create.foundation.block.IBE;
import com.verr1.controlcraft.registry.AIBlockEntities;
import com.verr1.controlcraft.unstable.ai.ui.AIUIFactory;
import net.createmod.catnip.gui.ScreenOpener;
import net.minecraft.core.BlockPos;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
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

public class CruiserBlock extends DirectionalBlock implements IBE<CruiserBlockEntity> {
    public static final String ID = "ai_cruiser";

    public CruiserBlock(Properties p) {
        super(p.strength(-1f, 3600000.0f));
    }

    @Override
    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        super.createBlockStateDefinition(builder);
        builder.add(FACING);
    }

    @Override
    public Class<CruiserBlockEntity> getBlockEntityClass() {
        return CruiserBlockEntity.class;
    }

    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p){
        ScreenOpener.open(AIUIFactory.createCruiserScreen(p));
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
    public BlockEntityType<? extends CruiserBlockEntity> getBlockEntityType() {
        return AIBlockEntities.CRUISER_BLOCKENTITY.get();
    }
}
