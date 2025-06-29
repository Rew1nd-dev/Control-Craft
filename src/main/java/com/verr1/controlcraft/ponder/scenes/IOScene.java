package com.verr1.controlcraft.ponder.scenes;

import com.simibubi.create.foundation.ponder.SceneBuilder;
import com.simibubi.create.foundation.ponder.SceneBuildingUtil;
import com.verr1.controlcraft.ponder.Constants;
import com.verr1.controlcraft.ponder.util.CimulinkPonderUtil;

import static com.verr1.controlcraft.ponder.scenes.BasicScene.*;

public class IOScene {

    public static void scene(SceneBuilder scene, SceneBuildingUtil util){
        var input = of(2, 1, 2);
        var analog = input.west();
        var output = input.east().east();
        var nixie = output.east();


        CimulinkPonderUtil cu = new CimulinkPonderUtil(scene, util);

        cu
                .init()
                .setBlock(Constants.INPUT, input).idle(4)
                .setBlock(Constants.OUTPUT, output).idle(4)
                .setBlock(Constants.ANALOG_LEVER, analog).idle(4)
                .setBlock(Constants.NIXIE, nixie).idle(4)
                .inst(addWire("io", input, output))
                .idle(READING_TIME);

        cu
                .text("Input port accepts surrounding redstone input", input, READING_TIME).idle(READING_TIME)
                .text("Output port applies redstone output to surrounding blocks", input, READING_TIME).idle(READING_TIME)
                .power(analog, 11)
                .showPower(nixie, 11).idle(READING_TIME)
                .text("You can also manually set values to input port from its ui", input, READING_TIME).idle(READING_TIME)
                .power(analog, 0)
                .showPower(nixie, 0).idle(5)
                .text("Set input to 4", input, READING_TIME).idle(READING_TIME)
                .showPower(nixie, 4).idle(5);

    }

}
