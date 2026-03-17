package com.verr1.controlcraft.content.blocks.flap;

import com.simibubi.create.content.contraptions.bearing.BearingBlock;
import com.simibubi.create.content.kinetics.base.DirectionalAxisKineticBlock;
import com.simibubi.create.foundation.block.IBE;
import com.simibubi.create.foundation.gui.ScreenOpener;
import com.tterrag.registrate.providers.DataGenContext;
import com.tterrag.registrate.providers.RegistrateBlockstateProvider;
import com.tterrag.registrate.util.nullness.NonNullBiConsumer;
import com.verr1.controlcraft.content.blocks.jet.JetBlockEntity;
import com.verr1.controlcraft.content.blocks.spatial.SpatialAnchorBlock;
import com.verr1.controlcraft.content.gui.factory.GenericUIFactory;
import com.verr1.controlcraft.foundation.api.common.ISignalAcceptor;
import com.verr1.controlcraft.registry.ControlCraftBlockEntities;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.DirectionalBlock;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.properties.EnumProperty;
import net.minecraft.world.level.block.state.properties.IntegerProperty;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.client.model.generators.ConfiguredModel;
import net.minecraftforge.client.model.generators.ModelFile;
import org.jetbrains.annotations.Nullable;

import java.util.function.BiFunction;

import static com.simibubi.create.foundation.data.BlockStateGen.directionalAxisBlock;

public class CompactFlapBlock extends DirectionalAxisKineticBlock implements
        ISignalAcceptor, IBE<CompactFlapBlockEntity>
{



    public static final String ID = "compact_flap";
    public static final IntegerProperty OFFSET = IntegerProperty.create("offset_mode", 0, 2);

    public CompactFlapBlock(Properties p_52591_) {
        super(p_52591_);
    }

    @Override
    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        builder.add(OFFSET);
        super.createBlockStateDefinition(builder);
    }

    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p){
        ScreenOpener.open(GenericUIFactory.createCompactFlapScreen(p));
    }

    @Override
    public InteractionResult onWrenched(BlockState state, UseOnContext context) {
        if(context.getClickedFace().getAxis().equals(state.getValue(FACING).getAxis())){
            return super.onWrenched(state, context);
        }
        if(context.getLevel().isClientSide)return InteractionResult.SUCCESS;
        context.getLevel().setBlock(context.getClickedPos(), state.cycle(OFFSET), 3);
        return InteractionResult.SUCCESS;
    }

    @Override
    public void neighborChanged(BlockState state, Level worldIn, BlockPos pos, Block blockIn, BlockPos fromPos,
                                boolean isMoving)  {
        ISignalAcceptor.super.onNeighborChanged(state, worldIn, pos, blockIn, fromPos, isMoving);
    }

    public static Direction getVerticalAxis(BlockState state){
        Direction facing = state.getValue(FACING);
        boolean axis = state.getValue(AXIS_ALONG_FIRST_COORDINATE);
        if(!facing.getAxis().equals(Direction.Axis.Y)){
            return Direction.UP;
        }

    }

    public static double getVerticalOffset(BlockState state){
        return 0.0;
    }

    @Override
    public InteractionResult use(BlockState state, Level worldIn, BlockPos pos, Player player, InteractionHand handIn, BlockHitResult hit){
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
    public Class<CompactFlapBlockEntity> getBlockEntityClass() {
        return CompactFlapBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends CompactFlapBlockEntity> getBlockEntityType() {
        return ControlCraftBlockEntities.COMPACT_FLAP_BLOCKENTITY.get();
    }

    public static class CompactFlapDataGenerator {
        public static <T extends DirectionalAxisKineticBlock> NonNullBiConsumer<DataGenContext<Block, T>, RegistrateBlockstateProvider> generate(){
            return
                (c, p) -> directionalAxisBlock(c, p, modelFunc(c, p));
        }

        private static <T extends DirectionalAxisKineticBlock> BiFunction<BlockState, Boolean, ModelFile> modelFunc(DataGenContext<Block, T> c, RegistrateBlockstateProvider p){
            return (state, vertical) -> {
                int off = state.getValue(OFFSET);
                String verticalFix = vertical ? "_n" : "_p";
                String flippedFix = off == 0 ? "_m" : off == 1 ? "_u" : "_d";
                String name = c.getName();
                return p.models().getExistingFile(p.modLoc("block/" + name + "/" + "block" + verticalFix + flippedFix));
            };
        }

    }

}
