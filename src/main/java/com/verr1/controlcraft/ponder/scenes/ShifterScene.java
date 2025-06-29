package com.verr1.controlcraft.ponder.scenes;

import com.simibubi.create.foundation.ponder.SceneBuilder;
import com.simibubi.create.foundation.ponder.SceneBuildingUtil;
import com.verr1.controlcraft.ponder.Constants;
import com.verr1.controlcraft.ponder.util.CimulinkPonderUtil;

import static com.verr1.controlcraft.ponder.scenes.BasicScene.*;

public class ShifterScene {

    public static void scene(SceneBuilder scene, SceneBuildingUtil util){
        var i = of(1, 1, 2);
        var shifter = i.east().east();
        var o = shifter.east().east();

        var lever = i.west();
        var nixie = o.east();
        var nixieIn = lever.north();

        CimulinkPonderUtil cu = new CimulinkPonderUtil(scene, util);

        cu
                .init()
                .setBlock(Constants.INPUT, i).idle(4)
                .setBlock(Constants.SHIFTER, shifter).idle(4)
                .setBlock(Constants.OUTPUT, o).idle(4)
                .setBlock(Constants.NIXIE, nixie, nixieIn).idle(4)
                .setBlock(Constants.ANALOG_LEVER, lever).idle(4)
                .inst(addWire("i", i, shifter)).idle(2)
                .inst(addWire("o", shifter, o)).idle(READING_TIME);

        cu
                .frame()
                .text("This is a Shifter", shifter, READING_TIME).idle(READING_TIME)
                .text("It shifts its input by DELAY tick, which is configurable in its ui", shifter, READING_TIME).idle(READING_TIME)
                .text("Let's say DELAY is now 20", shifter, READING_TIME)
                .frame()
                .power(lever, 4).showPower(nixieIn, 4).idle(5)
                .power(lever, 12).showPower(nixieIn, 12).idle(5)
                .power(lever, 8).showPower(nixieIn, 8).idle(5).idle(5)
                .showPower(nixie, 4).idle(5)
                .showPower(nixie, 12).idle(5)
                .showPower(nixie, 8).idle(READING_TIME);


    }

    public static void scene_1(SceneBuilder scene, SceneBuildingUtil util){
        var i = of(1, 1, 2);
        var clk = i.south().south();
        var shifter = i.east().east();
        var o = shifter.east().east();

        var lever = i.west();
        var leverClk = clk.west();
        var nixie = o.east();
        var nixieIn = lever.north();

        CimulinkPonderUtil cu = new CimulinkPonderUtil(scene, util);

        cu
                .init()
                .setBlock(Constants.INPUT, i, clk).idle(4)
                .setBlock(Constants.SHIFTER, shifter).idle(4)
                .setBlock(Constants.OUTPUT, o).idle(4)
                .setBlock(Constants.NIXIE, nixie, nixieIn).idle(4)
                .setBlock(Constants.ANALOG_LEVER, lever, leverClk).idle(4)
                .inst(addWire("i", i, shifter)).idle(2)
                .inst(addWire("clk", clk, shifter)).idle(2)
                .inst(addWire("o", shifter, o)).idle(READING_TIME);

        cu
                .frame()
                .text("This is an Async Shifter", shifter, READING_TIME).idle(READING_TIME)
                .text("It only shift its input when a positive edg is received at clk port", shifter, READING_TIME).idle(READING_TIME)
                .text("Let's say DELAY is now 4", shifter, READING_TIME)
                .frame()
                .power(lever, 4).showPower(nixieIn, 4).idle(5)
                .text("4-[0-0-0-0]-->0", shifter, READING_TIME).idle(READING_TIME)
                .power(leverClk, 11).idle(5).power(leverClk, 0).idle(5)
                .text("4-[4-0-0-0]-->0", shifter, READING_TIME).idle(READING_TIME)
                .power(lever, 8).showPower(nixieIn, 8).idle(5)
                .text("8-[4-0-0-0]-->0", shifter, READING_TIME).idle(READING_TIME)
                .power(lever, 12).showPower(nixieIn, 12).idle(5)
                .text("12-[4-0-0-0]-->0", shifter, READING_TIME).idle(READING_TIME)
                .power(leverClk, 11).idle(5).power(leverClk, 0).idle(5)
                .text("12-[12-4-0-0]-->0", shifter, READING_TIME).idle(READING_TIME)
                .power(leverClk, 11).idle(5).power(leverClk, 0).idle(5)
                .text("12-[12-12-4-0]-->0", shifter, READING_TIME).idle(READING_TIME)
                .power(leverClk, 11).showPower(nixie, 4).idle(5).power(leverClk, 0).idle(5)
                .text("12-[12-12-12-4]-->4", shifter, READING_TIME).idle(READING_TIME)
                .power(leverClk, 11).showPower(nixie, 12).idle(5).power(leverClk, 0).idle(5)
                .text("12-[12-12-12-12]-->12", shifter, READING_TIME).idle(READING_TIME);


    }

}
