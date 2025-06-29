package com.verr1.controlcraft.ponder;

import com.simibubi.create.AllBlocks;
import com.verr1.controlcraft.registry.CimulinkBlocks;
import com.verr1.controlcraft.registry.ControlCraftItems;
import net.minecraft.core.Direction;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.FaceAttachedHorizontalDirectionalBlock;
import net.minecraft.world.level.block.HorizontalDirectionalBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.AttachFace;

import static com.simibubi.create.content.kinetics.base.DirectionalKineticBlock.FACING;

public class Constants {

    public static final BlockState INPUT = CimulinkBlocks.INPUT.getDefaultState().setValue(FACING, Direction.UP);
    public static final BlockState OUTPUT = CimulinkBlocks.OUTPUT.getDefaultState().setValue(FACING, Direction.UP);
    public static final BlockState LOGIC = CimulinkBlocks.LOGIC_GATE.getDefaultState().setValue(FACING, Direction.UP);
    public static final BlockState FF = CimulinkBlocks.FF.getDefaultState().setValue(FACING, Direction.UP);
    public static final BlockState SHIFTER = CimulinkBlocks.SHIFTER.getDefaultState().setValue(FACING, Direction.UP);
    public static final BlockState FMA = CimulinkBlocks.FMA.getDefaultState().setValue(FACING, Direction.UP);
    public static final BlockState MUX = CimulinkBlocks.MUX.getDefaultState().setValue(FACING, Direction.UP);
    public static final BlockState CMP = CimulinkBlocks.COMPARATOR.getDefaultState().setValue(FACING, Direction.UP);
    public static final BlockState PROXY = CimulinkBlocks.PROXY.getDefaultState().setValue(FACING, Direction.UP);
    public static final BlockState CIRCUIT = CimulinkBlocks.CIRCUIT.getDefaultState().setValue(FACING, Direction.UP);
    public static final BlockState DC = CimulinkBlocks.DC.getDefaultState().setValue(FACING, Direction.UP);
    public static final BlockState FUNCTION = CimulinkBlocks.FUNCTIONS.getDefaultState().setValue(FACING, Direction.UP);
    public static final BlockState IMU = CimulinkBlocks.SENSOR.getDefaultState().setValue(FACING, Direction.UP);
    public static final BlockState SCOPE = CimulinkBlocks.SCOPE.getDefaultState().setValue(FACING, Direction.UP);

    public static final BlockState LEVER = Blocks.LEVER.defaultBlockState().setValue(FaceAttachedHorizontalDirectionalBlock.FACE, AttachFace.FLOOR);
    public static final BlockState LAMP = Blocks.REDSTONE_LAMP.defaultBlockState();

    public static final BlockState ANALOG_LEVER = AllBlocks.ANALOG_LEVER.getDefaultState();
    public static final BlockState NIXIE = AllBlocks.NIXIE_TUBES.get(DyeColor.RED).getDefaultState()
            .setValue(HorizontalDirectionalBlock.FACING, Direction.WEST);



    public static final ItemStack AWE = ControlCraftItems.ALL_IN_WAND.asStack();
    public static final ItemStack COMPILER = ControlCraftItems.CIRCUIT_COMPILER.asStack();
}
