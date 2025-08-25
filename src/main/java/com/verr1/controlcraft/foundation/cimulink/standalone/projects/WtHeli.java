package com.verr1.controlcraft.foundation.cimulink.standalone.projects;

import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Evaluator;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Val;

public class WtHeli {

    public static CircuitNbt pd3(){
        Evaluator eval = new Evaluator();

        Val cWx = eval.newVal("cWx");
        Val cWy = eval.newVal("cWy");
        Val cWz = eval.newVal("cWz");

        Val tWx = eval.newVal("tWx");
        Val tWy = eval.newVal("tWy");
        Val tWz = eval.newVal("tWz");

        Val g_tWx = eval.newVal("g_tWx");
        Val g_tWy = eval.newVal("g_tWy");
        Val g_tWz = eval.newVal("g_tWz");

        Val g_con_Wx = eval.newVal("g_con_Wx");
        Val g_con_Wy = eval.newVal("g_con_Wy");
        Val g_con_Wz = eval.newVal("g_con_Wz");

        Val conWx = g_con_Wx.mul(cWx.sub(g_tWx.mul(tWx)));
        Val conWy = g_con_Wy.mul(cWy.sub(g_tWy.mul(tWy)));
        Val conWz = g_con_Wz.mul(cWz.sub(g_tWz.mul(tWz)));

        eval.asOut("conWx", conWx).asOut("conWy", conWy).asOut("conWz", conWz);

        return eval.evaluate().buildContext();

    }

}
