package com.verr1.controlcraft.ponder.scenes;

import com.simibubi.create.foundation.ponder.SceneBuilder;
import com.simibubi.create.foundation.ponder.SceneBuildingUtil;
import com.verr1.controlcraft.content.links.logic.LogicGateBlock;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.GateTypes;
import com.verr1.controlcraft.ponder.Constants;
import com.verr1.controlcraft.ponder.scenes.BasicScene.*;
import com.verr1.controlcraft.ponder.util.CimulinkPonderUtil;

import static com.verr1.controlcraft.ponder.scenes.BasicScene.*;

public class GatesScene {


    public static void scene(SceneBuilder scene, SceneBuildingUtil util){

        var i0 = of(2, 1, 2);
        var i1 = i0.south().south();

        var gate = i0.south().east();
        var o = gate.east();

        var analog0 = i0.west();
        var analog1 = i1.west();

        var nixie = o.east();

        CimulinkPonderUtil cu = new CimulinkPonderUtil(scene, util);

        cu
                .init()
                .setBlock(Constants.INPUT, i0, i1).idle(4)
                .setBlock(Constants.LOGIC, gate).idle(4)
                .setBlock(Constants.OUTPUT, o).idle(4)
                .setBlock(Constants.NIXIE, nixie).idle(4)
                .setBlock(Constants.ANALOG_LEVER, analog1, analog0).idle(4)
                .inst(addWire("i0", i0, gate).inIndex(0, 2)).idle(2)
                .inst(addWire("i1", i1, gate).inIndex(1, 2)).idle(2)
                .inst(addWire("o", gate, o)).idle(READING_TIME)
                .frame()
                .text("Logic Gates Do Logical Computations", gate, READING_TIME).idle(READING_TIME)
                .text("Any value > 0.5 is consider a \"True\" input", gate, READING_TIME).idle(READING_TIME)
                .text("Otherwise it is consider a \"False\" input", gate, READING_TIME).idle(READING_TIME)
                .text("Gates output is always either 1.0 or 0.0", gate, READING_TIME).idle(READING_TIME);


        cu
                .frame()
                .text("This is an AND gate", gate, READING_TIME).idle(READING_TIME)
                .power(analog0, 4)
                .power(analog1, 0)
                .text("i0 = 4(True)", analog0, READING_TIME / 3).idle(READING_TIME / 3)
                .text("i1 = 0(False)", analog1, READING_TIME / 3).idle(READING_TIME / 3)
                .text("o = 0(False)", o, READING_TIME / 3).idle(READING_TIME / 2)
                .power(analog0, 0)
                .power(analog1, 4)
                .text("i0 = 0(False)", analog0, READING_TIME / 3).idle(READING_TIME / 3)
                .text("i1 = 4(True)", analog1, READING_TIME / 3).idle(READING_TIME / 3)
                .text("o = 0(False)", o, READING_TIME / 3).idle(READING_TIME / 2)
                .power(analog0, 0)
                .power(analog1, 0)
                .text("i0 = 0(False)", analog0, READING_TIME / 3).idle(READING_TIME / 3)
                .text("i1 = 0(False)", analog1, READING_TIME / 3).idle(READING_TIME / 3)
                .text("o = 0(False)", o, READING_TIME / 3).idle(READING_TIME / 2)
                .power(analog0, 4)
                .power(analog1, 4)
                .showPower(nixie, 1)
                .text("i0 = 4(True)", analog0, READING_TIME / 3).idle(READING_TIME / 3)
                .text("i1 = 4(True)", analog1, READING_TIME / 3).idle(READING_TIME / 3)
                .text("o = 1(True)", o, READING_TIME / 3).idle(READING_TIME / 2)
                .text("AND Gate Only Output True(1.0) when Both Input are True(>0.5)", gate, READING_TIME).idle(READING_TIME);

        cu
                .frame()
                .setBlock(s -> s.setValue(LogicGateBlock.TYPE, GateTypes.OR), gate).idle(READING_TIME / 3)
                .text("This is an OR gate", gate, READING_TIME).idle(READING_TIME)
                .power(analog0, 4)
                .power(analog1, 0)
                .showPower(nixie, 1)
                .text("i0 = 4(True)", analog0, READING_TIME / 3).idle(READING_TIME / 3)
                .text("i1 = 0(False)", analog1, READING_TIME / 3).idle(READING_TIME / 3)
                .text("o = 1(True)", o, READING_TIME / 2).idle(READING_TIME / 2)
                .power(analog0, 0)
                .power(analog1, 4)
                .showPower(nixie, 1)
                .text("i0 = 0(False)", analog0, READING_TIME / 3).idle(READING_TIME / 3)
                .text("i1 = 4(True)", analog1, READING_TIME / 3).idle(READING_TIME / 3)
                .text("o = 1(True)", o, READING_TIME / 3).idle(READING_TIME / 2)
                .power(analog0, 0)
                .power(analog1, 0)
                .showPower(nixie, 0)
                .text("i0 = 0(False)", analog0, READING_TIME / 3).idle(READING_TIME / 3)
                .text("i1 = 0(False)", analog1, READING_TIME / 3).idle(READING_TIME / 3)
                .text("o = 0(False)", o, READING_TIME / 3).idle(READING_TIME / 2)
                .power(analog0, 4)
                .power(analog1, 4)
                .showPower(nixie, 1)
                .text("i0 = 4(True)", analog0, READING_TIME / 3).idle(READING_TIME / 3)
                .text("i1 = 4(True)", analog1, READING_TIME / 3).idle(READING_TIME / 3)
                .text("o = 1(True)", o, READING_TIME / 3).idle(READING_TIME / 2)
                .text("OR Gate Only Output False(0.0) when Both Input are False(<0.5)", gate, READING_TIME).idle(READING_TIME);

        cu
                .frame()
                .setBlock(s -> s.setValue(LogicGateBlock.TYPE, GateTypes.XOR), gate).idle(READING_TIME / 3)
                .text("This is an XOR gate", gate, READING_TIME).idle(READING_TIME)
                .power(analog0, 4)
                .power(analog1, 0)
                .showPower(nixie, 1)
                .text("i0 = 4(True)", analog0, READING_TIME / 3).idle(READING_TIME / 3)
                .text("i1 = 0(False)", analog1, READING_TIME / 3).idle(READING_TIME / 3)
                .text("o = 1(True)", o, READING_TIME / 3).idle(READING_TIME / 2)
                .power(analog0, 0)
                .power(analog1, 4)
                .showPower(nixie, 1)
                .text("i0 = 0(False)", analog0, READING_TIME / 3).idle(READING_TIME / 3)
                .text("i1 = 4(True)", analog1, READING_TIME / 3).idle(READING_TIME / 3)
                .text("o = 1(True)", o, READING_TIME / 3).idle(READING_TIME / 2)
                .power(analog0, 0)
                .power(analog1, 0)
                .showPower(nixie, 0)
                .text("i0 = 0(False)", analog0, READING_TIME / 3).idle(READING_TIME / 3)
                .text("i1 = 0(False)", analog1, READING_TIME / 3).idle(READING_TIME / 3)
                .text("o = 0(False)", o, READING_TIME / 3).idle(READING_TIME / 2)
                .power(analog0, 4)
                .power(analog1, 4)
                .showPower(nixie, 0)
                .text("i0 = 4(True)", analog0, READING_TIME / 3).idle(READING_TIME / 3)
                .text("i1 = 4(True)", analog1, READING_TIME / 3).idle(READING_TIME / 3)
                .text("o = 0(False)", o, READING_TIME / 3).idle(READING_TIME / 2)
                .text("XOR Gate Only Output True(1.0) when Only 1 Input are True(>0.5)", gate, READING_TIME).idle(READING_TIME);


        cu
                .frame()
                .inst(removeWire("i1"))
                .setBlock(s -> s.setValue(LogicGateBlock.TYPE, GateTypes.NOT), gate).idle(READING_TIME / 3)
                .text("This is an NOT gate", gate, READING_TIME).idle(READING_TIME)
                .power(analog0, 4).showPower(nixie, 0)
                .text("i0 = 4(True)", analog0, READING_TIME / 3).idle(READING_TIME / 2)
                .text("o = 0(False)", o, READING_TIME / 3).idle(READING_TIME / 2)
                .power(analog0, 0).showPower(nixie, 1).idle(READING_TIME)
                .text("i0 = 0(False)", analog0, READING_TIME / 3).idle(READING_TIME / 2)
                .text("o = 1(True)", o, READING_TIME / 3).idle(READING_TIME / 2)
                .text("NOT Gate flips its input", gate, READING_TIME).idle(READING_TIME);

    }



}
