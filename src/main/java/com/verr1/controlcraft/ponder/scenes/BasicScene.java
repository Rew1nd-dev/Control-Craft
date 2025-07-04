package com.verr1.controlcraft.ponder.scenes;

import com.simibubi.create.foundation.ponder.PonderPalette;
import com.simibubi.create.foundation.ponder.SceneBuilder;
import com.simibubi.create.foundation.ponder.SceneBuildingUtil;
import com.simibubi.create.foundation.ponder.Selection;
import com.simibubi.create.foundation.ponder.instruction.ReplaceBlocksInstruction;
import com.simibubi.create.foundation.ponder.instruction.WorldModifyInstruction;
import com.verr1.controlcraft.ponder.Constants;
import com.verr1.controlcraft.ponder.instructions.CimulinkWireInstruction;
import com.verr1.controlcraft.ponder.util.CimulinkPonderUtil;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.LeverBlock;
import net.minecraft.world.level.block.RedstoneLampBlock;
import net.minecraft.world.level.block.state.BlockState;

import java.util.function.UnaryOperator;

public class BasicScene {
    public static final String EMPTY = "cimulink/base";
    public static final int READING_TIME = 60;
    public static final int READING_TIME_133 = (int)(READING_TIME * 1.333);
    public static final int READING_TIME_80 = (int)(READING_TIME * 0.8);
    public static final int READING_TIME_50 = (int)(READING_TIME * 0.5);
    public static final int READING_TIME_33 = (int)(READING_TIME * 0.3333);
    public static final int READING_TIME_25 = (int)(READING_TIME * 0.25);
    public static final int READING_TIME_20 = (int)(READING_TIME * 0.2);

    public static void scene_0(SceneBuilder scene, SceneBuildingUtil util){
        var input = of(2, 1, 2);
        var output = of(4, 1, 2);
        var air = of(6, 1, 2);

        CimulinkPonderUtil cu = new CimulinkPonderUtil(scene, util, "basic_0");

        cu
                .init()
                .setBlock(Constants.INPUT, input)
                .setBlock(Constants.OUTPUT, output);


        cu
                .frame()
                .text("Every component has input ports or output ports", input, READING_TIME).idle(READING_TIME)
                .text("Every component has input ports or output ports", output, READING_TIME).idle(READING_TIME)
                .text("link components with wand", input, READING_TIME).idle(READING_TIME)
                .click(input, READING_TIME / 3).idle(READING_TIME / 3)
                .click(output, READING_TIME / 3).idle(READING_TIME / 3)
                .text("click air to confirm", air, READING_TIME).idle(READING_TIME)
                .inst(CimulinkWireInstruction.add("w0").fromTo(input, output)).idle(READING_TIME)
                .frame()

        ;
        cu.end();
    }

    public static void scene_1(SceneBuilder scene, SceneBuildingUtil util){
        var input_0 = of(2, 1, 2);
        var input_1 = of(2, 1, 4);
        var output_0 = of(4, 1, 2);
        var output_1 = of(4, 1, 3);
        var output_2 = of(4, 1, 4);


        CimulinkPonderUtil cu = new CimulinkPonderUtil(scene, util, "basic_1");

        cu
                .init()
                .setBlock(Constants.INPUT, input_0, input_1)
                .setBlock(Constants.OUTPUT, output_0, output_1, output_2);

        cu
                .text("An output port can link to multiple input ports", input_0, READING_TIME).idle(READING_TIME)
                .inst(addWire("i0", input_0, output_0)).idle(10)
                .inst(addWire("i1", input_0, output_1)).idle(10)
                .inst(addWire("i2", input_0, output_2)).idle(30)
                .frame()
                .text("But an input port can only link to one output port", output_1, READING_TIME).idle(READING_TIME)
                .inst(addWire("i3", input_1, output_0)).idle(10)
                .inst(removeWire("i0")).idle(10)
                .inst(addWire("i4", input_1, output_1)).idle(10)
                .inst(removeWire("i1")).idle(10)
                .inst(addWire("i5", input_1, output_2)).idle(10)
                .inst(removeWire("i2")).idle(10);

        cu.end();
    }


    public static void scene_2(SceneBuilder scene, SceneBuildingUtil util){
        int startZ = 1;
        int startX = 1;

        var ff = of(startX, 1, startZ);
        var shifter = ff.south();

        var logic = ff.east().east().east();
        var fma = logic.south();
        var mux = fma.south();
        var cmp = logic.east();
        var dc = cmp.south();
        var func = dc.south();
        var in = cmp.east();
        var out = in.south();

        var empty = ff.east().east();

        var circuit = mux.south().west();
        var imu = circuit.west();
        var proxy = imu.south();

        var chains = new BlockPos[4];

        var chain_i = of(0, 1, 1);
        var lever = chain_i.south();


        chains[0] = chain_i.east().east();

        for (int i = 1; i < chains.length; i++){
            chains[i] = chains[i - 1].east();
        }


        var chain_o = chains[chains.length - 1].east().south();
        var lamp = chain_o.south();

        CimulinkPonderUtil cu = new CimulinkPonderUtil(scene, util, "basic_2");

        cu
                .init()
                .setBlock(Constants.FF, ff)
                .setBlock(Constants.SHIFTER, shifter)
                .setBlock(Constants.LOGIC, logic)
                .setBlock(Constants.FMA, fma)
                .setBlock(Constants.MUX, mux)
                .setBlock(Constants.CMP, cmp)
                .setBlock(Constants.DC, dc)
                .setBlock(Constants.FUNCTION, func)
                .setBlock(Constants.CIRCUIT, circuit)
                .setBlock(Constants.IMU, imu)
                .setBlock(Constants.PROXY, proxy)
                .setBlock(Constants.OUTPUT, out)
                .setBlock(Constants.INPUT, in);

        cu
                .text("There Are 3 Kinds Of Components", empty, READING_TIME).idle(READING_TIME)
                .text("Combinational Components (Yellow Panel)", fma, READING_TIME).idle(READING_TIME)
                .text("Temporal Components (Blue Panel)", shifter, READING_TIME).idle(READING_TIME)
                .text("Special Components (Other Color)", proxy, READING_TIME).idle(READING_TIME)
                .frame()
                .clearBlock(of(0,1,0), of(7, 1, 7)).idle(10);

        cu
                .setBlock(Constants.FMA, chains)
                .setBlock(Constants.INPUT, chain_i)
                .setBlock(Constants.OUTPUT, chain_o)
                .setBlock(Constants.LEVER, lever)
                .setBlock(Constants.LAMP, lamp);

        for (BlockPos chain : chains) {
            cu.setBlock(Constants.FMA, chain);
        }


        cu
                .inst(addWire("chain_i", chain_i, chains[0])).idle(4)
                .inst(addWire("chain_o", chains[chains.length - 1], chain_o)).idle(4);

        for (int i = 0; i < chains.length - 1; i++) {
            cu.inst(addWire("chain_" + i, chains[i], chains[i + 1])).idle(4);
        }

        cu
                .frame()
                .text("Combinational Components are components that do not have any internal state", chains[0], READING_TIME).idle(READING_TIME)
                .text("They Transmits Signals Without Any Delay", chains[0], READING_TIME).idle(READING_TIME)
                .setBlock(s -> s.setValue(LeverBlock.POWERED, true), lever)
                .setBlock(s -> s.setValue(RedstoneLampBlock.LIT, true), lamp).idle(20)
                .setBlock(s -> s.setValue(LeverBlock.POWERED, false), lever)
                .setBlock(s -> s.setValue(RedstoneLampBlock.LIT, false), lamp).idle(20);

        for (BlockPos chain : chains) {
            cu.setBlock(Constants.FF, chain).idle(5);
        }

        cu
                .idle(READING_TIME)
                .frame()
                .text("Temporal Components has internal state", chains[0], READING_TIME).idle(READING_TIME)
                .text("They takes time to transit between states", chains[0], READING_TIME).idle(READING_TIME)
                .setBlock(s -> s.setValue(LeverBlock.POWERED, true), lever).idle(20)
                .setBlock(s -> s.setValue(RedstoneLampBlock.LIT, true), lamp).idle(20)
                .text("The delay is decided by specific settings", chains[0], READING_TIME).idle(READING_TIME);




        cu.end();
    }

    public static void scene_3(SceneBuilder scene, SceneBuildingUtil util){
        int size = 4;

        var chains = new BlockPos[size];

        var chain_i = of(1, 1, 1);
        var lever = chain_i.west();


        chains[0] = chain_i.east().east();

        for (int i = 1; i < chains.length; i++){
            chains[i] = chains[i - 1].south();
        }

        CimulinkPonderUtil cu = new CimulinkPonderUtil(scene, util, "basic_3");

        cu
                .init()
                .setBlock(Constants.INPUT, chain_i).idle(20)
                .setBlock(Constants.LEVER, lever).idle(20);
                // .setBlock(Constants.SHIFTER, chains).idle(20);

        for (BlockPos chain : chains) {
            cu.setBlock(Constants.SHIFTER, chain).idle(10);
        }

        cu.inst(addWire("chain_i", chain_i, chains[0]).inIndex(1, 2)).idle(4);
        for (int i = 0; i < size - 1; i++) {
            cu.inst(addWire("chain_" + i, chains[i], chains[i + 1])).idle(4);
        }
        cu.inst(addWire("chain_loop", chains[size - 1], chains[0]).inIndex(0, 2)).idle(READING_TIME);

        cu
                .selectArea(chains[0], chains[size - 1], PonderPalette.GREEN, "loop", 2 * READING_TIME).idle(READING_TIME / 2)
                .text("This is a loop", chains[0], (int) (READING_TIME * 1.5)).idle((int) (READING_TIME * 1.5))
                .text("A loop must contain at least 1 temporal component", chains[0], READING_TIME).idle(READING_TIME);

        for (int i = 0; i < size - 1; i++) {
            cu.setBlock(Constants.LOGIC, chains[i]).idle(10);
        }

        cu.text("This is still a valid loop", chains[0], READING_TIME).idle(READING_TIME);



        cu.setBlock(Constants.LOGIC, chains[size - 1]).idle(10);

        cu
                .text("This is not a valid loop", chains[0], READING_TIME).idle(READING_TIME)
                .text("If you input something...", chains[0], READING_TIME / 2).idle(READING_TIME / 2);

        cu
                .setBlock(power(true), lever).idle(READING_TIME / 4)
                .inst(removeWire("chain_loop")).idle(READING_TIME / 3)
                .text("the loop will disconnect automatically", chains[0], READING_TIME).idle(READING_TIME);
        cu.end();
    }

    public static UnaryOperator<BlockState> power(boolean on){
        return s -> s.setValue(LeverBlock.POWERED, on);
    }

    public static UnaryOperator<BlockState> lit(boolean on){
        return s -> s.setValue(RedstoneLampBlock.LIT, on);
    }


    public static CimulinkWireInstruction addWire(Object slot, BlockPos from, BlockPos to){
        return CimulinkWireInstruction.add(slot)
                .fromTo(from, to)
                .eternal();
    }

    public static CimulinkWireInstruction.Remove removeWire(Object slot){
        return CimulinkWireInstruction.remove(slot);
    }

    public static BlockPos of(int x, int y, int z){
        return new BlockPos(x, y, z);
    }

    public static WorldModifyInstruction set(Selection selection, BlockState block) {
        return new ReplaceBlocksInstruction(selection, $ -> block, true, false);
    }

    public static WorldModifyInstruction set(Selection selection, UnaryOperator<BlockState> block) {
        return new ReplaceBlocksInstruction(selection, block, true, true);
    }


}
