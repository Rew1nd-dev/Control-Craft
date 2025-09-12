package com.verr1.controlcraft.foundation.cimulink.standalone.projects;

import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Evaluator;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Val;

public class View {

    public static CircuitNbt create(){
        Evaluator eval = new Evaluator();

        Val degIn = eval.newVal("isDegIn");
        Val deg2rad = eval.newVal(Math.PI / 180);
        Val convertIn = eval.orElse(degIn, eval.newVal(1), deg2rad);
        Val yaw = eval.newVal("yaw");
        Val pitch = eval.newVal("pitch");
        Val g_yaw = eval.newVal("g_yaw");
        Val o_yaw = eval.newVal("o_yaw");
        Val g_pitch = eval.newVal("g_pitch");
        Val o_pitch = eval.newVal("o_pitch");

        Val yaw_c = yaw.div(convertIn).mul(g_yaw).add(o_yaw).mul(deg2rad);
        Val pitch_c = pitch.div(convertIn).mul(g_pitch).add(o_pitch).mul(deg2rad);

        eval.asOut("yaw_c", yaw_c).asOut("pitch_c", pitch_c);

        return eval.evaluate().buildContext();
    }

}
