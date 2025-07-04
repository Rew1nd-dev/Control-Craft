package com.verr1.controlcraft.content.blocks.receiver;

import com.simibubi.create.foundation.block.IBE;
import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.gui.factory.GenericUIFactory;
import com.verr1.controlcraft.content.legacy.PeripheralInterfaceBlockEntity_;
import com.verr1.controlcraft.registry.ControlCraftBlockEntities;
import net.minecraft.core.BlockPos;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.DirectionalBlock;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.shapes.CollisionContext;
import net.minecraft.world.phys.shapes.VoxelShape;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import static com.verr1.controlcraft.registry.ControlCraftShapes.HALF_BOX_BASE;


public class PeripheralInterfaceBlock extends DirectionalBlock implements IBE<PeripheralInterfaceBlockEntity> {

    public static final String ID = "receiver";

    public PeripheralInterfaceBlock(Properties p_52591_) {
        super(p_52591_);
    }

    @Override
    public void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        builder.add(FACING);
        super.createBlockStateDefinition(builder);
    }

    @OnlyIn(Dist.CLIENT)
    protected void displayScreen(BlockPos pos){
        ScreenOpener.open(GenericUIFactory.createPeripheralInterfaceScreen(pos));
    }

    @Override
    public void onRemove(BlockState state, Level worldIn, BlockPos pos, BlockState newState, boolean isMoving){
        IBE.onRemove(state, worldIn, pos, newState);
    }

    @Override
    public BlockState getStateForPlacement(BlockPlaceContext context) {
        return defaultBlockState().setValue(FACING, context.getClickedFace());
    }

    @Override
    public VoxelShape getShape(BlockState state, BlockGetter worldIn, BlockPos pos, CollisionContext context) {
        return HALF_BOX_BASE.get(state.getValue(FACING));
    }

    @Override
    public void onPlace(BlockState state, Level world, BlockPos pos, BlockState oldState, boolean isMoving) {
        if(world.isClientSide)return;
        ControlCraftServer.SERVER_EXECUTOR.executeLater(
                () -> {
                    BlockEntity entity = world.getExistingBlockEntity(pos);
                    if(entity instanceof PeripheralInterfaceBlockEntity_ receiver){
                        receiver.updateAttachedPeripheral();
                    }
                },
                    5
        );
    }



    @Override
    public InteractionResult use(BlockState state, Level worldIn, BlockPos pos, Player player, InteractionHand handIn,
                                 BlockHitResult hit){
        if(     worldIn.isClientSide
                && handIn == InteractionHand.MAIN_HAND
                && player.getItemInHand(InteractionHand.MAIN_HAND).isEmpty()
                && !player.isShiftKeyDown()
        ) {
            displayScreen(pos);
            return InteractionResult.PASS;
        }
        return InteractionResult.PASS;
    }

    @Override
    public void neighborChanged(BlockState state, Level world, BlockPos pos, Block otherBlock, BlockPos neighborPos,
                                boolean isMoving) {
        if(world.isClientSide)return;
        withBlockEntityDo(world, pos, PeripheralInterfaceBlockEntity::updateAttachedPeripheral);
    }

    @Override
    public Class<PeripheralInterfaceBlockEntity> getBlockEntityClass() {
        return PeripheralInterfaceBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends PeripheralInterfaceBlockEntity> getBlockEntityType() {
        return ControlCraftBlockEntities.RECEIVER_BLOCKENTITY.get();
    }




}
