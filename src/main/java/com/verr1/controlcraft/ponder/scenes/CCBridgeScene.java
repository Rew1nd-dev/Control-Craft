package com.verr1.controlcraft.ponder.scenes;

import com.simibubi.create.foundation.ponder.SceneBuilder;
import com.simibubi.create.foundation.ponder.SceneBuildingUtil;
import com.verr1.controlcraft.ponder.Constants;
import com.verr1.controlcraft.ponder.util.CimulinkPonderUtil;

import static com.verr1.controlcraft.ponder.scenes.BasicScene.*;

public class CCBridgeScene {


    public static void scene(SceneBuilder scene, SceneBuildingUtil util){

        var computer = of(2, 1, 2);
        var bridge = of(2, 2, 2);

        CimulinkPonderUtil cu = new CimulinkPonderUtil(scene, util, "ccBridge");

        cu
                .init()
                .setBlock(Constants.COMPUTER, computer).idle(4)
                .setBlock(Constants.CC_BRIDGE, bridge).idle(READING_TIME / 2);

        cu
                .text("ComputerCraft is a mod adds programmable computers", computer, READING_TIME * 2).idle(READING_TIME)
                .text("CC-Bridge block can connect Cimulink circuits to ComputerCraft computers", bridge, READING_TIME * 3).idle(READING_TIME)
                .text("It can read and write values to/from circuits", bridge, READING_TIME * 2).idle(READING_TIME)
                .text("You can use Lua language to program the computer", computer, READING_TIME * 2).idle(READING_TIME)
                .frame()
                .text("Find CC Bridge peripheral by calling: peripheral.find(\"cc_link_bridge\")", computer, READING_TIME * 2).idle(READING_TIME)
                .frame()
                .text("Call setInput(index, value) to set input values", computer, READING_TIME * 2).idle(READING_TIME)
                .frame()
                .text("Call getOutput(index) to get output values", computer, READING_TIME * 2).idle(READING_TIME)
                .frame()
                .text("Index can not be greater than 7 or lower than 0", computer, READING_TIME).idle(READING_TIME);

        cu
                .end();


    }

}
