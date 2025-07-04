package com.verr1.controlcraft.ponder.scenes;

import com.simibubi.create.foundation.ponder.PonderPalette;
import com.simibubi.create.foundation.ponder.SceneBuilder;
import com.simibubi.create.foundation.ponder.SceneBuildingUtil;
import com.verr1.controlcraft.ponder.Constants;
import com.verr1.controlcraft.ponder.util.CimulinkPonderUtil;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.phys.Vec3;

import static com.verr1.controlcraft.ponder.scenes.BasicScene.*;

public class CircuitScene {

    public static void scene(SceneBuilder scene, SceneBuildingUtil util){
        var circuit = of(4, 1, 5);
        var circuitI = circuit.west();
        var circuitO = circuit.east();

        var in = of(2, 1, 2);
        var d_shift = in.east().south();
        var d_fma = d_shift.east();
        var pid_fma = d_fma.north();
        var i_shift = pid_fma.north();
        var i_fma = i_shift.west();
        var out = pid_fma.east();


        var lu = in.north();
        var rd = out.south();

        CimulinkPonderUtil cu = new CimulinkPonderUtil(scene, util, "circuit_0");

        cu
                .init()
                .setBlock(Constants.INPUT, in).idle(4)
                .setBlock(Constants.OUTPUT, out).idle(4)
                .setBlock(Constants.SHIFTER, d_shift, i_shift).idle(4)
                .setBlock(Constants.FMA, d_fma, pid_fma, i_fma).idle(READING_TIME / 4)
                .inst(addWire("id", in, d_shift)).idle(4)
                .inst(addWire("ds_df", d_shift, d_fma).inIndex(0, 2)).idle(4)
                .inst(addWire("df_pid_s", d_fma, pid_fma).inIndex(0, 3)).idle(4)
                .inst(addWire("if_is", i_fma, i_shift)).idle(4)
                .inst(addWire("is_if", i_shift, i_fma).inIndex(0, 2)).idle(4)
                .inst(addWire("is_pid_s", i_shift, pid_fma).inIndex(1, 3)).idle(4)
                .inst(addWire("i_ds",in, d_fma).inIndex(1, 2)).idle(4)
                .inst(addWire("i_pid_s", in, pid_fma).inIndex(2, 3)).idle(4)
                .inst(addWire("i_if", in, i_fma).inIndex(1, 2))
                .inst(addWire("o", pid_fma, out)).idle(READING_TIME / 2);


        cu
                .text("This is a working circuit", pid_fma, READING_TIME / 2).idle(READING_TIME / 2)
                .text("But it is too big...", pid_fma, READING_TIME / 2).idle(READING_TIME / 2)
                .text("Compiler is design for this case", pid_fma, READING_TIME).idle(READING_TIME);

        cu
                .frame()
                .text("use wand in compiler mode", pid_fma, READING_TIME_80).idle(READING_TIME_80)
                .text("select an area to include circuit", pid_fma, READING_TIME).idle(READING_TIME)
                .click(lu, READING_TIME / 4).idle(READING_TIME / 4)
                .click(rd, READING_TIME / 4).idle(READING_TIME / 4)
                .selectArea(lu, rd, PonderPalette.GREEN, "circuit area", READING_TIME).idle(READING_TIME)
                .text("right-click air to confirm", rd, READING_TIME).idle(READING_TIME)
                .click(rd, READING_TIME / 4).idle(READING_TIME / 4);

        var e = scene.world.createItemEntity(pid_fma.getCenter(), new Vec3(0, 0.2, 0), Constants.COMPILER);

        cu
                .text("Now you get a compiler, which stores the circuit", pid_fma, READING_TIME).idle(READING_TIME);

        scene.world.modifyEntity(e, Entity::discard);

        cu
                .frame()
                .setBlock(Constants.CIRCUIT, circuit).idle(READING_TIME_25)
                .text("Place a uncompiled circuit block", circuit, READING_TIME_80).idle(READING_TIME_80)
                .text("Click with compiler you just got", circuit, READING_TIME_80).idle(READING_TIME_80)
                .click(circuit, Constants.COMPILER, READING_TIME / 4).idle(READING_TIME / 2)
                .text("Now it copies the circuit you just store", circuit, READING_TIME).idle(READING_TIME);


        cu
                .frame()
                .setBlock(Constants.INPUT, circuitI).idle(4)
                .setBlock(Constants.OUTPUT, circuitO).idle(4)
                .inst(addWire("ci", circuitI, circuit)).idle(4)
                .inst(addWire("co", circuit, circuitO)).idle(READING_TIME)
                .text("Input Link Port blocks of the original circuit", in, READING_TIME_80).idle(READING_TIME_80)
                .text("Is converted to circuit block's input ports", circuit, READING_TIME_80).idle(READING_TIME_80)
                .text("Output Link Port blocks of the original circuit", out, READING_TIME_80).idle(READING_TIME_80)
                .text("Is converted to circuit block's output ports", circuit, READING_TIME_80).idle(READING_TIME_80);

        cu.end();
    }

    public static void scene_1(SceneBuilder scene, SceneBuildingUtil util){
        var circuit0 = of(3, 1, 3);
        var circuit1 = circuit0.west();
        var fma = circuit0.north();
        var circuitI0 = circuit1.west();
        var circuitI1 = circuitI0.south();
        var circuitO0 = circuit0.east();
        var circuitO1 = circuitO0.south();

        var compiled = circuit0.south().south();
        var compiled_i0 = compiled.west();
        var compiled_i1 = compiled_i0.south();
        var compiled_o0 = compiled.east();
        var compiled_o1 = compiled_o0.south();

        CimulinkPonderUtil cu = new CimulinkPonderUtil(scene, util, "circuit_1");

        cu
                .init()
                .setBlock(Constants.CIRCUIT, circuit0, circuit1).idle(4)
                .setBlock(Constants.FMA, fma).idle(4)
                .setBlock(Constants.INPUT, circuitI0, circuitI1).idle(4)
                .setBlock(Constants.OUTPUT, circuitO0, circuitO1).idle(4)
                .inst(addWire("i0", circuitI0, fma).inIndex(0, 2)).idle(4)
                .inst(addWire("i1", circuitI1, fma).inIndex(0, 2)).idle(4)
                .inst(addWire("f0", fma, circuit0)).idle(4)
                .inst(addWire("f1", circuit0, circuit1)).idle(4)
                .inst(addWire("c0", circuit1, circuitO0).outIndex(0, 2)).idle(4)
                .inst(addWire("c1", circuit1, circuitO1).outIndex(1, 2)).idle(READING_TIME);

        cu
                .frame()
                .text("Compiled Circuit Can Be Further Compiled.", circuit0, READING_TIME).idle(READING_TIME)
                .selectArea(circuitI0, circuitO1, PonderPalette.GREEN, "circuit area", READING_TIME).idle(READING_TIME)
                .setBlock(Constants.CIRCUIT, compiled).idle(READING_TIME / 2)
                .click(compiled, Constants.COMPILER, READING_TIME / 4).idle(READING_TIME / 2)
                .setBlock(Constants.INPUT, compiled_i0, compiled_i1).idle(4)
                .setBlock(Constants.OUTPUT, compiled_o0, compiled_o1).idle(4)
                .inst(addWire("ci0", compiled_i0, compiled).inIndex(0, 2)).idle(4)
                .inst(addWire("ci1", compiled_i1, compiled).inIndex(1, 2)).idle(4)
                .inst(addWire("co0", compiled, compiled_o0).outIndex(0, 2)).idle(4)
                .inst(addWire("co1", compiled, compiled_o1).outIndex(1, 2)).idle(READING_TIME);

        cu
                .text("This circuit behaves just the same as the original one", compiled, READING_TIME).idle(READING_TIME);

        cu.end();
    }

}
