package com.verr1.controlcraft.content.blocks.flap;

import com.simibubi.create.content.contraptions.ITransformableBlock;
import com.simibubi.create.content.contraptions.StructureTransform;
import com.simibubi.create.content.contraptions.bearing.BearingBlock;
import com.simibubi.create.content.kinetics.base.DirectionalAxisKineticBlock;
import com.simibubi.create.content.kinetics.base.IRotate;
import com.simibubi.create.foundation.block.IBE;
import com.simibubi.create.foundation.gui.ScreenOpener;
import com.simibubi.create.foundation.utility.Iterate;
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
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.LevelReader;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.DirectionalBlock;
import net.minecraft.world.level.block.Rotation;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.properties.BooleanProperty;
import net.minecraft.world.level.block.state.properties.EnumProperty;
import net.minecraft.world.level.block.state.properties.IntegerProperty;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.client.model.generators.ConfiguredModel;
import net.minecraftforge.client.model.generators.ModelFile;
import org.jetbrains.annotations.Nullable;

import java.util.function.BiFunction;

import static com.simibubi.create.content.kinetics.base.DirectionalAxisKineticBlock.AXIS_ALONG_FIRST_COORDINATE;
import static com.simibubi.create.foundation.data.BlockStateGen.directionalAxisBlock;

public class CompactFlapBlock extends BearingBlock implements
        ISignalAcceptor, IBE<CompactFlapBlockEntity> , ITransformableBlock
{

    public static final String ID = "compact_flap";
    public static final IntegerProperty OFFSET = IntegerProperty.create("offset_mode", 0, 2);
    public static final BooleanProperty INVISIBLE = BooleanProperty.create("invisible");

    public CompactFlapBlock(Properties p_52591_) {
        super(p_52591_);
        registerDefaultState(defaultBlockState()
            .setValue(INVISIBLE, false)
            .setValue(OFFSET, 0)
        );
    }

    protected Direction getFacingForPlacement(BlockPlaceContext context) {
        Direction facing = context.getNearestLookingDirection()
            .getOpposite();
        if (context.getPlayer() != null && context.getPlayer()
            .isShiftKeyDown())
            facing = facing.getOpposite();
        return facing;
    }

    protected boolean getAxisAlignmentForPlacement(BlockPlaceContext context) {
        return context.getHorizontalDirection()
            .getAxis() == Direction.Axis.X;
    }

    @Override
    public InteractionResult onSneakWrenched(BlockState state, UseOnContext context) {
        Level world = context.getLevel();
        BlockPos pos = context.getClickedPos();
        world.setBlock(pos, state.cycle(INVISIBLE), 3);
        return InteractionResult.SUCCESS;
    }

    @Override
    public BlockState getStateForPlacement(BlockPlaceContext context) {
        Direction facing = getFacingForPlacement(context);
        BlockPos pos = context.getClickedPos();
        Level world = context.getLevel();
        boolean alongFirst = false;
        Direction.Axis faceAxis = facing.getAxis();

        if (faceAxis.isHorizontal()) {
            alongFirst = faceAxis == Direction.Axis.Z;
            Direction positivePerpendicular = faceAxis == Direction.Axis.X ? Direction.SOUTH : Direction.EAST;

            boolean shaftAbove = prefersConnectionTo(world, pos, Direction.UP, true);
            boolean shaftBelow = prefersConnectionTo(world, pos, Direction.DOWN, true);
            boolean preferLeft = prefersConnectionTo(world, pos, positivePerpendicular, false);
            boolean preferRight = prefersConnectionTo(world, pos, positivePerpendicular.getOpposite(), false);

            if (shaftAbove || shaftBelow || preferLeft || preferRight)
                alongFirst = faceAxis == Direction.Axis.X;
        }

        if (faceAxis.isVertical()) {
            alongFirst = getAxisAlignmentForPlacement(context);
            Direction prefferedSide = null;

            for (Direction side : Iterate.horizontalDirections) {
                if (!prefersConnectionTo(world, pos, side, true)
                    && !prefersConnectionTo(world, pos, side.getClockWise(), false))
                    continue;
                if (prefferedSide != null && prefferedSide.getAxis() != side.getAxis()) {
                    prefferedSide = null;
                    break;
                }
                prefferedSide = side;
            }

            if (prefferedSide != null)
                alongFirst = prefferedSide.getAxis() == Direction.Axis.X;
        }

        return this.defaultBlockState()
            .setValue(FACING, facing)
            .setValue(AXIS_ALONG_FIRST_COORDINATE, alongFirst);
    }

    protected boolean prefersConnectionTo(LevelReader reader, BlockPos pos, Direction facing, boolean shaftAxis) {
        if (!shaftAxis)
            return false;
        BlockPos neighbourPos = pos.relative(facing);
        BlockState blockState = reader.getBlockState(neighbourPos);
        Block block = blockState.getBlock();
        return block instanceof IRotate
            && ((IRotate) block).hasShaftTowards(reader, neighbourPos, blockState, facing.getOpposite());
    }

    @Override
    public Direction.Axis getRotationAxis(BlockState state) {
        Direction.Axis pistonAxis = state.getValue(FACING)
            .getAxis();
        boolean alongFirst = state.getValue(AXIS_ALONG_FIRST_COORDINATE);

        if (pistonAxis == Direction.Axis.X)
            return alongFirst ? Direction.Axis.Y : Direction.Axis.Z;
        if (pistonAxis == Direction.Axis.Y)
            return alongFirst ? Direction.Axis.X : Direction.Axis.Z;
        if (pistonAxis == Direction.Axis.Z)
            return alongFirst ? Direction.Axis.X : Direction.Axis.Y;

        throw new IllegalStateException("Unknown axis??");
    }

    @Override
    public BlockState rotate(BlockState state, Rotation rot) {
        if (rot.ordinal() % 2 == 1)
            state = state.cycle(AXIS_ALONG_FIRST_COORDINATE);
        return super.rotate(state, rot);
    }

    public BlockState transform(BlockState state, StructureTransform transform) {
        if (transform.mirror != null) {
            state = mirror(state, transform.mirror);
        }

        if (transform.rotationAxis == Direction.Axis.Y) {
            return rotate(state, transform.rotation);
        }

        Direction newFacing = transform.rotateFacing(state.getValue(FACING));
        if (transform.rotationAxis == newFacing.getAxis() && transform.rotation.ordinal() % 2 == 1) {
            state = state.cycle(AXIS_ALONG_FIRST_COORDINATE);
        }
        return state.setValue(FACING, newFacing);
    }

    @Override
    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        builder.add(OFFSET, AXIS_ALONG_FIRST_COORDINATE, INVISIBLE);
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
            boolean vertical = axis && facing.getAxis() == Direction.Axis.X || !axis && facing.getAxis() == Direction.Axis.Z;
            Direction left =  facing.getClockWise(Direction.Axis.Y);
            return vertical ? left : Direction.UP;
        }
        Direction dir0 = axis ? Direction.NORTH : Direction.EAST;
        return facing.getAxisDirection() == Direction.AxisDirection.POSITIVE ? dir0 : dir0.getOpposite();
    }

    public static double getVerticalOffset(BlockState state){
        int off = state.getValue(OFFSET);
        return off == 0 ? 0 : off == 2 ? 0.5 : -0.5;
    }

    @Override
    public InteractionResult use(BlockState state, Level worldIn, BlockPos pos, Player player, InteractionHand handIn, BlockHitResult hit){
        ItemStack heldItem = player.getItemInHand(handIn);
        if (!heldItem.isEmpty() && heldItem.getItem() instanceof BlockItem blockItem) {
            if (!(worldIn.getBlockEntity(pos) instanceof CompactFlapBlockEntity be)) {
                return InteractionResult.PASS;
            }
            if (worldIn.isClientSide) {
                return InteractionResult.SUCCESS;
            }

            be.setRenderMaterial(blockItem.getBlock().defaultBlockState());
            return InteractionResult.SUCCESS;
        }

        if(     worldIn.isClientSide
                && handIn == InteractionHand.MAIN_HAND
                && heldItem.isEmpty()
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
        public static NonNullBiConsumer<DataGenContext<Block, CompactFlapBlock>, RegistrateBlockstateProvider> generate(){
            return
                (c, p) -> directionalAxisBlock(c, p, modelFunc(c, p));
        }

        private static <T extends CompactFlapBlock> BiFunction<BlockState, Boolean, ModelFile> modelFunc(DataGenContext<Block, T> c, RegistrateBlockstateProvider p){
            return (state, vertical) -> {
                String name = c.getName();
                int off = state.getValue(OFFSET);
                boolean invisible = state.getValue(INVISIBLE);
                if(invisible){
                    return p.models().getExistingFile(p.modLoc("block/" + name + "/" + "block_invisible"));
                }
                String verticalFix = vertical ? "_n" : "_p";
                String flippedFix = off == 0 ? "_m" : off == 1 ? "_u" : "_d";

                return p.models().getExistingFile(p.modLoc("block/" + name + "/" + "block" + verticalFix + flippedFix));
            };
        }



        public static void directionalAxisBlock(DataGenContext<Block, CompactFlapBlock> ctx,
                                                                                        RegistrateBlockstateProvider prov, BiFunction<BlockState, Boolean, ModelFile> modelFunc) {
            prov.getVariantBuilder(ctx.getEntry())
                .forAllStates(state -> {

                    boolean alongFirst = state.getValue(DirectionalAxisKineticBlock.AXIS_ALONG_FIRST_COORDINATE);
                    Direction direction = state.getValue(DirectionalAxisKineticBlock.FACING);
                    boolean vertical = direction.getAxis()
                        .isHorizontal() && (direction.getAxis() == Direction.Axis.X) == alongFirst;
                    int xRot = direction == Direction.DOWN ? 270 : direction == Direction.UP ? 90 : 0;
                    int yRot = direction.getAxis()
                        .isVertical() ? alongFirst ? 0 : 90 : (int) direction.toYRot();

                    return ConfiguredModel.builder()
                        .modelFile(modelFunc.apply(state, vertical))
                        .rotationX(xRot)
                        .rotationY(yRot)
                        .build();
                });
        }

    }

}
