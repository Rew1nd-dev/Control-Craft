package com.verr1.controlcraft.ponder.scenes;

import com.simibubi.create.foundation.ponder.SceneBuilder;
import com.simibubi.create.foundation.ponder.SceneBuildingUtil;
import com.verr1.controlcraft.ponder.Constants;
import com.verr1.controlcraft.ponder.util.CimulinkPonderUtil;

import static com.verr1.controlcraft.ponder.scenes.BasicScene.*;

public class CombinationalScene {

    public static void fma(SceneBuilder scene, SceneBuildingUtil util) {
        var i0 = of(2, 1, 2);
        var i1 = i0.south().south();
        var fma = i0.east().east();
        var o = fma.east().east();

        var lever0 = i0.west();
        var lever1 = i1.west();
        var nixieO = o.north();
        var nixieI0 = lever0.west();
        var nixieI1 = lever1.west();

        CimulinkPonderUtil cu = new CimulinkPonderUtil(scene, util);

        cu
                .init()
                .setBlock(Constants.INPUT, i0, i1).idle(4)
                .setBlock(Constants.FMA, fma).idle(4)
                .setBlock(Constants.OUTPUT, o).idle(4)
                .setBlock(Constants.NIXIE, nixieO, nixieI0, nixieI1).idle(4)
                .setBlock(Constants.ANALOG_LEVER, lever0, lever1).idle(4)
                .inst(addWire("i0", i0, fma).inIndex(0, 2)).idle(2)
                .inst(addWire("i1", i1, fma).inIndex(1, 2)).idle(2)
                .inst(addWire("o", fma, o)).idle(READING_TIME);

        cu
                .frame()
                .text("This is a linear adder", fma, READING_TIME).idle(READING_TIME)
                .text("It multiply its input with coefficients", fma, READING_TIME).idle(READING_TIME)
                .text("And add them together", fma, READING_TIME).idle(READING_TIME)
                .text("default coeffs are all 1.0", fma, READING_TIME).idle(READING_TIME)
                .power(lever0, 4).showPower(nixieI0, 4).idle(15)
                .power(lever1, 2).showPower(nixieI1, 2).idle(15)
                .showPower(nixieO, 6).showPower(nixieO, 4).idle(15)
                .text("i0 = 4", i0, READING_TIME / 3).idle(READING_TIME / 3)
                .text("i1 = 2", i1, READING_TIME / 3).idle(READING_TIME / 3)
                .text("o =4*1.0+2*1.0=6", o, READING_TIME / 2).idle(READING_TIME / 2)
                .power(lever0, 0).showPower(nixieI0, 4).idle(5)
                .power(lever1, 0).showPower(nixieI1, 2).idle(5)
                .showPower(nixieO, 0).showPower(nixieO, 0).idle(15);

        cu
                .frame()
                .text("coefficients can be changed", fma, READING_TIME).idle(READING_TIME)
                .text("Let's change i0 coefficient to -1.0", fma, READING_TIME).idle(READING_TIME)
                .power(lever0, 2).showPower(nixieI0, 2).idle(15)
                .power(lever1, 4).showPower(nixieI1, 4).idle(15)
                .showPower(nixieO, 2).showPower(nixieO, 2).idle(15)
                .text("i0 = 2", i0, READING_TIME / 3).idle(READING_TIME / 3)
                .text("i1 = 4", i1, READING_TIME / 3).idle(READING_TIME / 3)
                .text("o =2*-1.0+4*1.0=2", o, READING_TIME / 2).idle(READING_TIME / 2)
                .text("A Subtractor!", fma, READING_TIME).idle(READING_TIME);


    }

    public static void mux(SceneBuilder scene, SceneBuildingUtil util){
        var i0 = of(2, 1, 2);
        var i1 = i0.south().south();

        var mux = i0.east().south();
        var sel = mux.south();
        var o = mux.east();

        var lever0 = i0.west();
        var lever1 = i1.west();
        var leverSel = sel.south();
        var nixieO = o.east();
        var nixieI0 = lever0.west();
        var nixieI1 = lever1.west();

        CimulinkPonderUtil cu = new CimulinkPonderUtil(scene, util);

        cu
                .init()
                .setBlock(Constants.INPUT, i0, i1, sel).idle(4)
                .setBlock(Constants.MUX, mux).idle(4)
                .setBlock(Constants.OUTPUT, o).idle(4)
                .setBlock(Constants.NIXIE, nixieO, nixieI0, nixieI1).idle(4)
                .setBlock(Constants.ANALOG_LEVER, lever0, lever1, leverSel).idle(4)
                .inst(addWire("i0", i0, mux).inIndex(0, 3)).idle(2)
                .inst(addWire("i1", i1, mux).inIndex(1, 3)).idle(2)
                .inst(addWire("sel", sel, mux).inIndex(2, 3)).idle(2)
                .inst(addWire("o", mux, o)).idle(READING_TIME);

        cu
                .frame()
                .text("This is a Selector", mux, READING_TIME).idle(READING_TIME)
                .text("It selects one of its inputs based on a selector", mux, READING_TIME).idle(READING_TIME)
                .text("If selector is False, it outputs i0", sel, READING_TIME).idle(READING_TIME)
                .text("If selector is True, it outputs i1", sel, READING_TIME).idle(READING_TIME)
                .power(lever0, 4).showPower(nixieI0, 4).idle(15)
                .power(lever1, 2).showPower(nixieI1, 2).idle(15)
                .power(leverSel, 0).showPower(nixieO, 4).idle(15)
                .text("i0 = 4", i0, READING_TIME / 3).idle(READING_TIME / 3)
                .text("i1 = 2", i1, READING_TIME / 3).idle(READING_TIME / 3)
                .text("sel = 0(False)", sel, READING_TIME / 3).idle(READING_TIME / 3)
                .text("o = 4", o, READING_TIME / 2).idle(READING_TIME / 2)
                .power(leverSel, 1).showPower(nixieO, 2).idle(15)
                .text("sel = 1(True)", sel, READING_TIME / 3).idle(READING_TIME / 3)
                .text("o = 2", o, READING_TIME / 2).idle(READING_TIME / 2);
    }


    public static void cmp(SceneBuilder scene, SceneBuildingUtil util){
        var i0 = of(2, 1, 2);
        var i1 = i0.south().south();
        var cmp = i0.east().south();
        var eq = cmp.east();
        var ge = eq.south();
        var le = eq.north();

        var lever0 = i0.west();
        var lever1 = i1.west();
        var lampEq = eq.east();
        var lampGe = ge.east();
        var lampLe = le.east();
        var nixieI0 = lever0.west();
        var nixieI1 = lever1.west();

        CimulinkPonderUtil cu = new CimulinkPonderUtil(scene, util);

        cu
                .init()
                .setBlock(Constants.INPUT, i0, i1).idle(4)
                .setBlock(Constants.CMP, cmp).idle(4)
                .setBlock(Constants.OUTPUT, eq, ge, le).idle(4)
                .setBlock(Constants.LAMP, lampEq, lampGe, lampLe).idle(4)
                .setBlock(Constants.NIXIE, nixieI0, nixieI1).idle(4)
                .setBlock(Constants.ANALOG_LEVER, lever0, lever1).idle(4)
                .inst(addWire("i0", i0, cmp).inIndex(0, 2)).idle(2)
                .inst(addWire("i1", i1, cmp).inIndex(1, 2)).idle(2)
                .inst(addWire("eq", cmp, eq).outIndex(2, 3)).idle(2)
                .inst(addWire("ge", cmp, ge).outIndex(3, 3)).idle(2)
                .inst(addWire("le", cmp, le).outIndex(4, 3)).idle(2);

        cu
                .frame()
                .text("This is a Comparator", cmp, READING_TIME).idle(READING_TIME)
                .text("It compares its inputs", cmp, READING_TIME).idle(READING_TIME)
                .text("The result is its three outputs(>/=/<)", cmp, READING_TIME).idle(READING_TIME)
                .power(lever0, 4).showPower(nixieI0, 4)
                .power(lever1, 2).showPower(nixieI1, 2)
                .setBlock(lit(true), lampGe)
                .setBlock(lit(false), lampEq)
                .setBlock(lit(false), lampLe)
                .idle(10)
                .text("i0(4) > i1(2), Ge = true, other = false", i0, READING_TIME / 3).idle(READING_TIME / 3)
                .power(lever0, 2).showPower(nixieI0, 2)
                .power(lever1, 4).showPower(nixieI1, 4)
                .setBlock(lit(false), lampGe)
                .setBlock(lit(false), lampEq)
                .setBlock(lit(true), lampLe)
                .idle(10)
                .text("i0(2) < i1(4), Le = true, other = false", i0, READING_TIME / 3).idle(READING_TIME / 3)
                .power(lever0, 4).showPower(nixieI0, 4)
                .power(lever1, 4).showPower(nixieI1, 2)
                .setBlock(lit(false), lampGe)
                .setBlock(lit(true), lampEq)
                .setBlock(lit(false), lampLe)
                .idle(10)
                .text("|i0(4) - i1(4)| < 1e-3, Eq = true, other = false", i0, READING_TIME / 3).idle(READING_TIME / 3);


    }

}
