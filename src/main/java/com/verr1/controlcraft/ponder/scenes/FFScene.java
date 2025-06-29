package com.verr1.controlcraft.ponder.scenes;

import com.simibubi.create.foundation.ponder.SceneBuilder;
import com.simibubi.create.foundation.ponder.SceneBuildingUtil;
import com.verr1.controlcraft.content.links.ff.FFBlock;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.FFTypes;
import com.verr1.controlcraft.ponder.Constants;
import com.verr1.controlcraft.ponder.util.CimulinkPonderUtil;

import static com.verr1.controlcraft.ponder.scenes.BasicScene.*;

public class FFScene {

    public static void scene(SceneBuilder scene, SceneBuildingUtil util){
        var i0 = of(2, 1, 2);
        var i1 = i0.south().south();

        var gate = i0.south().east();
        var o = gate.east();

        var lever0 = i0.west();
        var lever1 = i1.west();

        var nixie = o.east();

        CimulinkPonderUtil cu = new CimulinkPonderUtil(scene, util);

        cu
                .init()
                .setBlock(Constants.INPUT, i0, i1).idle(4)
                .setBlock(Constants.FF, gate).idle(4)
                .setBlock(Constants.OUTPUT, o).idle(4)
                .setBlock(Constants.NIXIE, nixie).idle(4)
                .setBlock(Constants.ANALOG_LEVER, lever0, lever1).idle(4)
                .inst(addWire("i0", i0, gate)).idle(2)
                .inst(addWire("o", gate, o)).idle(4)
                .showPower(nixie, 0).idle(READING_TIME);


        cu
                .frame()
                .text("This is a T-Flip-Flop", gate, READING_TIME).idle(READING_TIME)
                .text("It Flips its input every tick, with 1 tick delay", gate, READING_TIME).idle(READING_TIME)
                .power(lever0, 4).idle(1).showPower(nixie, 0).idle(READING_TIME)
                .text("i = 4(True)", i0, READING_TIME / 2)
                .text("o = 0(False)", o, READING_TIME / 2).idle(READING_TIME / 2)
                .power(lever0, 0).idle(1).showPower(nixie, 1).idle(READING_TIME)
                .text("i = 0(False)", i0, READING_TIME / 2)
                .text("o = 1(True)", o, READING_TIME / 2).idle(READING_TIME / 2);

        cu
                .frame()
                .setBlock(s -> s.setValue(FFBlock.TYPE, FFTypes.D_FF), gate).idle(READING_TIME)
                .text("This is a D-Flip-Flop", gate, READING_TIME).idle(READING_TIME)
                .text("It transmits its input with 1 tick delay", gate, READING_TIME).idle(READING_TIME)
                .power(lever0, 4).idle(1).showPower(nixie, 1).idle(READING_TIME)
                .text("i = 4(True)", i0, READING_TIME / 2)
                .text("o = 1(True)", o, READING_TIME / 2).idle(READING_TIME / 2)
                .power(lever0, 0).idle(1).showPower(nixie, 0).idle(READING_TIME)
                .text("i = 0(False)", i0, READING_TIME / 2)
                .text("o = 0(False)", o, READING_TIME / 2).idle(READING_TIME / 2);

        /*
        cu
                .frame()
                .setBlock(s -> s.setValue(FFBlock.TYPE, FFTypes.RS_FF), gate).idle(READING_TIME)
                .inst(addWire("i0", i0, gate).inIndex(0, 2)).idle(2)
                .inst(addWire("i1", i1, gate).inIndex(1, 2)).idle(2)
                .text("This is a RS-Flip-Flop", gate, READING_TIME).idle(READING_TIME)
                .text("It sets its output to 1 when R is 1, and resets it to 0 when S is 1", gate, READING_TIME).idle(READING_TIME)
                .text("with 1 tick delay", gate, READING_TIME).idle(READING_TIME)
                .power(lever0, 4).idle(1).showPower(nixie, 1).idle(READING_TIME)
                .power(lever0, 0).idle(1).showPower(nixie, 0).idle(READING_TIME)
                .power(lever1, 4).idle(1).showPower(nixie, 0).idle(READING_TIME)
                .power(lever1, 0).idle(1).showPower(nixie, 1).idle(READING_TIME);

        cu
                .frame()
                .setBlock(s -> s.setValue(FFBlock.TYPE, FFTypes.JK_FF), gate).idle(READING_TIME)
                .inst(addWire("i0", i0, gate).inIndex(0, 2)).idle(2)
                .inst(addWire("i1", i1, gate).inIndex(1, 2)).idle(2)
                .text("This is a JK-Flip-Flop", gate, READING_TIME).idle(READING_TIME)
                .text("Input port J", i0, READING_TIME).idle(READING_TIME)
                .text("Input port K", i1, READING_TIME).idle(READING_TIME)
                .text("It sets its output to 1 when J is 1, and resets it to 0 when K is 1", gate, READING_TIME).idle(READING_TIME)
                .text("When J = K = 1, it flips its output every tick", gate, READING_TIME).idle(READING_TIME)
                .power(lever0, 4).idle(1).showPower(nixie, 1).idle(READING_TIME) // j
                .power(lever0, 0).idle(1).showPower(nixie, 0).idle(READING_TIME)
                .power(lever1, 0).idle(1).showPower(nixie, 0).idle(READING_TIME) // k
                .power(lever1, 1).idle(1).showPower(nixie, 0).idle(READING_TIME)
                .frame();


        cu.power(lever1, 4).power(lever0, 4).idle(1);

        for(int i = 0; i < 40; i++){
            cu.showPower(nixie, i % 2 == 0 ? 1 : 0).idle(1);
        }

        cu.power(lever1, 0).power(lever0, 0).showPower(nixie, 0).idle(1);
        * */






    }

    public static void scene_1(SceneBuilder scene, SceneBuildingUtil util){
        var i0 = of(2, 1, 2);
        var clk = i0.south().south();

        var gate = i0.south().east();
        var o = gate.east();

        var leverIn = i0.west();
        var leverClk = clk.west();

        var nixie = o.east();

        CimulinkPonderUtil cu = new CimulinkPonderUtil(scene, util);

        cu
                .init()
                .setBlock(Constants.INPUT, i0, clk).idle(4)
                .setBlock(Constants.FF, gate).idle(4)
                .setBlock(Constants.OUTPUT, o).idle(4)
                .setBlock(Constants.NIXIE, nixie).idle(4)
                .setBlock(Constants.ANALOG_LEVER, leverIn, leverClk).idle(4)
                .inst(addWire("i", i0, gate).inIndex(0, 2)).idle(2)
                .inst(addWire("clk", clk, gate).inIndex(1, 2)).idle(2)
                .inst(addWire("o", gate, o)).idle(4)
                .showPower(nixie, 0).idle(READING_TIME);

        cu
                .frame()
                .text("This is an Async T-Flip-Flop, It Has a clk input", gate, READING_TIME).idle(READING_TIME)
                .text("This is clk port", clk, READING_TIME).idle(READING_TIME)
                .text("This is input port", i0, READING_TIME).idle(READING_TIME)
                .text("Only When clk is change from False to True, i.e. an positive edge", clk, READING_TIME).idle(READING_TIME)
                .text("Will TFF flip its input", gate, READING_TIME).idle(READING_TIME)
                .frame()
                .power(leverIn, 4).idle(READING_TIME / 4)
                .text("output = 0.0", o, READING_TIME / 2).idle(READING_TIME / 2)
                .power(leverIn, 0).idle(READING_TIME / 4)
                .text("output = 0.0", o, READING_TIME / 2).idle(READING_TIME / 2)
                .power(leverIn, 4).idle(READING_TIME / 4)
                .text("output = 0.0", o, READING_TIME / 2).idle(READING_TIME)
                .power(leverClk, 0)
                .showPower(nixie, 0).idle(READING_TIME)
                .text("Clk hasn't change", i0, READING_TIME / 2).idle(READING_TIME / 2)
                .text("TFF holds its output (0.0)", o, READING_TIME / 2).idle(READING_TIME / 2)
                .power(leverIn, 0).idle(READING_TIME / 4)
                .text("Input is 0(False)", i0, READING_TIME / 2).idle(READING_TIME)
                .power(leverClk, 0).idle(4)
                .power(leverClk, 11)
                .showPower(nixie, 1).idle(READING_TIME)
                .text("A Positive Edge is received", clk, READING_TIME / 2).idle(READING_TIME)
                .text("TFF flip its input and get output 1.0(True)", o, READING_TIME / 2).idle(READING_TIME)
                .power(leverIn, 4).idle(READING_TIME / 4)
                .text("Input is 4(True)", i0, READING_TIME / 2).idle(READING_TIME)
                .power(leverClk, 0).idle(4)
                .power(leverClk, 11)
                .showPower(nixie, 0).idle(READING_TIME)
                .text("A Positive Edge is received", clk, READING_TIME / 2).idle(READING_TIME)
                .text("TFF flip its input and get output 0.0(False)", o, READING_TIME / 2).idle(READING_TIME);

        cu
                .frame()
                .setBlock(s -> s.setValue(FFBlock.TYPE, FFTypes.D_FF), gate).idle(READING_TIME)
                .text("This is an Async D-Flip-Flop, It Has a clk input", gate, READING_TIME).idle(READING_TIME)
                .text("Only When clk is change from False to True, i.e. an positive edge", clk, READING_TIME).idle(READING_TIME)
                .text("Will DFF transmit its input", gate, READING_TIME).idle(READING_TIME)
                .frame()
                .power(leverIn, 4).idle(READING_TIME / 4)
                .text("output = 0.0", o, READING_TIME / 2).idle(READING_TIME / 2)
                .power(leverIn, 0).idle(READING_TIME / 4)
                .text("output = 0.0", o, READING_TIME / 2).idle(READING_TIME / 2)
                .power(leverIn, 4).idle(READING_TIME / 4)
                .text("output = 0.0", o, READING_TIME / 2).idle(READING_TIME / 2)
                .power(leverClk, 0)
                .showPower(nixie, 0).idle(READING_TIME)
                .text("Clk hasn't change", i0, READING_TIME / 2).idle(READING_TIME / 2)
                .text("DFF holds its output (0.0)", o, READING_TIME / 2).idle(READING_TIME)
                .power(leverIn, 4).idle(READING_TIME / 4)
                .text("Input is 4(True)", i0, READING_TIME / 2).idle(READING_TIME)
                .power(leverClk, 0).idle(4)
                .power(leverClk, 11)
                .showPower(nixie, 1).idle(READING_TIME)
                .text("A Positive Edge is received", clk, READING_TIME / 2).idle(READING_TIME)
                .text("DFF transmit its input and get output 1.0(True)", o, READING_TIME / 2).idle(READING_TIME)
                .power(leverIn, 0).idle(READING_TIME / 4)
                .text("Input is 0(False)", i0, READING_TIME / 2).idle(READING_TIME)
                .power(leverClk, 0).idle(4)
                .power(leverClk, 11)
                .showPower(nixie, 0).idle(READING_TIME)
                .text("A Positive Edge is received", clk, READING_TIME / 2).idle(READING_TIME)
                .text("DFF transmit its input and get output 0.0(False)", o, READING_TIME / 2).idle(READING_TIME);


    }

}
