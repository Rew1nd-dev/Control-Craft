package com.verr1.controlcraft.unstable.blocks.explosive;

import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.compact.createbigcannons.CreateBigCannonsCompact;
import com.verr1.controlcraft.foundation.api.common.ISignalHandler;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.unstable.AIServer;
import com.verr1.controlcraft.unstable.ai.ui.AIUIFactory;
import com.verr1.controlcraft.unstable.blocks.autocannon.AiAutocannonBlockEntity;
import com.verr1.controlcraft.utils.VSGetterUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.Explosion;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.DirectionalBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.jetbrains.annotations.NotNull;
import org.joml.Vector3dc;


public class ExplosiveBlock extends DirectionalBlock implements ISignalHandler{

    public static final String ID = "ai_explosive";

    public ExplosiveBlock(Properties p_49795_) {
        super(p_49795_.noLootTable());
    }

    @Override
    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        super.createBlockStateDefinition(builder);
        builder.add(FACING);
    }



    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p){
        // ScreenOpener.open(AIUIFactory.createAutoCannonScreen(p));
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
    public void onBlockExploded(BlockState state, Level level, BlockPos pos, Explosion explosion) {
        super.onBlockExploded(state, level, pos, explosion);
        detonate(level, pos);
    }

    @Override
    public BlockState getStateForPlacement(BlockPlaceContext context) {
        return defaultBlockState().setValue(FACING, context.getClickedFace());
    }


    public static void detonate(Level level, BlockPos pos){
        Vector3dc actual = VSGetterUtils.getAbsolutePosition(WorldBlockPos.of(level, pos));

        CreateBigCannonsCompact.createExplosion(
                level, actual.x(), actual.y(), actual.z(), 16, false, Level.ExplosionInteraction.BLOCK
        );

        level.setBlock(pos, Blocks.AIR.defaultBlockState(), 3);

    }


    @Override
    public void accept(Level level, BlockState state, BlockPos pos, Direction direction, int strength) {
        if(level.isClientSide)return;
        var s = VSGetterUtils.getShip(level, pos).orElse(null);
        long id = s == null ? -1 : s.getId();
        boolean doNot = AIServer.MANAGER.isInPool(id);
        if(!doNot && strength > 0)detonate(level, pos);
    }
}
