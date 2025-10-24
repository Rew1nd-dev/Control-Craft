package com.verr1.controlcraft.foundation.cimulink.standalone.projects;

import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Evaluator;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Val;

public class Flights {

    public static CircuitNbt flight4(){
        Evaluator eval = new Evaluator();
        Val nOne = eval.newVal(-1);
        Val one = eval.newVal(1);

        Val pitch = eval.newVal("pitch");
        Val yaw = eval.newVal("yaw");
        Val roll = eval.newVal("roll");
        Val vt = eval.newVal("vel");
        Val vf = eval.newVal("velFeed");

        Val rate = eval.newVal("rate").max(one);

        Val g_vel = eval.newVal("g_vel");
        Val g_feed = eval.newVal("g_vfeed");
        Val g_con = eval.newVal("g_vcon");



        Val g_pitch = eval.newVal("g_pitch");
        Val g_yaw = eval.newVal("g_yaw");
        Val g_roll = eval.newVal("g_roll");

        Val g_right = eval.newVal("g_right");
        Val g_left = eval.newVal("g_left");
        Val g_mid = eval.newVal("g_mid");

        Val g_pitch_b = eval.newVal("g_pitch_b");
        Val g_yaw_b = eval.newVal("g_yaw_b");
        Val g_roll_b = eval.newVal("g_roll_b");

        Val g_right_b = eval.newVal("g_right_b");
        Val g_left_b = eval.newVal("g_left_b");
        Val g_mid_b = eval.newVal("g_mid_b");

        Val pitchIn = eval.clamp(pitch, nOne, one).power(rate);
        Val yawIn = eval.clamp(yaw, nOne, one).power(rate);
        Val rollIn = eval.clamp(roll, nOne, one).power(rate);

        Val gained_pitch = pitchIn.mul(g_pitch);
        Val gained_yaw = yawIn.mul(g_yaw);
        Val gained_roll = rollIn.mul(g_roll);

        Val gained_pitch_b = pitchIn.mul(g_pitch_b);
        Val gained_yaw_b = yawIn.mul(g_yaw_b);
        Val gained_roll_b = rollIn.mul(g_roll_b);

        Val right = gained_pitch.add(gained_roll).mul(g_right);
        Val left = gained_pitch.sub(gained_roll).mul(g_left);
        Val mid = gained_yaw.mul(g_mid);

        Val right_b = gained_pitch_b.add(gained_roll_b).mul(g_right_b);
        Val left_b = gained_pitch_b.sub(gained_roll_b).mul(g_left_b);
        Val mid_b = gained_yaw_b.mul(g_mid_b);


        Val clamped_gCon = eval.orElse(vt.lessThan(eval.newVal(0.2)), g_con, eval.newVal(0));

        Val vCon = (vt.mul(g_vel).sub(vf.mul(g_feed))).mul(clamped_gCon);

        eval.asOut("right", right).asOut("left", left).asOut("mid", mid).asOut("vcon", vCon)
                .asOut("right_b", right_b).asOut("left_b", left_b).asOut("mid_b", mid_b);

        return eval.evaluate().buildContext();
    }


    public static CircuitNbt flight6(){
        Evaluator eval = new Evaluator();
        Val nOne = eval.newVal(-1);
        Val one = eval.newVal(1);

        Val pitch = eval.newVal("pitch");
        Val yaw = eval.newVal("yaw");
        Val roll = eval.newVal("roll");
        Val vt = eval.newVal("vel");
        Val vf = eval.newVal("velFeed");

        Val rate = eval.newVal("rate").max(one);

        Val g_vel = eval.newVal("g_vel");
        Val g_feed = eval.newVal("g_vfeed");
        Val g_con = eval.newVal("g_vcon");

        Val mdv = eval.newVal("g_mdv");

        Val g_pitch = eval.newVal("g_pitch");
        Val g_yaw = eval.newVal("g_yaw");
        Val g_roll = eval.newVal("g_roll");

        Val g_right = eval.newVal("g_right");
        Val g_left = eval.newVal("g_left");
        Val g_mid = eval.newVal("g_mid");

        Val g_pitch_b = eval.newVal("g_pitch_b");
        Val g_yaw_b = eval.newVal("g_yaw_b");
        Val g_roll_b = eval.newVal("g_roll_b");

        Val g_right_b = eval.newVal("g_right_b");
        Val g_left_b = eval.newVal("g_left_b");
        Val g_mid_b = eval.newVal("g_mid_b");

        Val pitchIn = eval.clamp(pitch, nOne, one).power(rate);
        Val yawIn = eval.clamp(yaw, nOne, one).power(rate);
        Val rollIn = eval.clamp(roll, nOne, one).power(rate);

        Val gained_pitch = pitchIn.mul(g_pitch);
        Val gained_yaw = yawIn.mul(g_yaw);
        Val gained_roll = rollIn.mul(g_roll);

        Val gained_pitch_b = pitchIn.mul(g_pitch_b);
        Val gained_yaw_b = yawIn.mul(g_yaw_b);
        Val gained_roll_b = rollIn.mul(g_roll_b);

        Val right = gained_pitch.add(gained_roll).mul(g_right);
        Val left = gained_pitch.sub(gained_roll).mul(g_left);
        Val mid = gained_yaw.mul(g_mid);

        Val right_b = gained_pitch_b.add(gained_roll_b).mul(g_right_b);
        Val left_b = gained_pitch_b.sub(gained_roll_b).mul(g_left_b);
        Val mid_b = gained_yaw_b.mul(g_mid_b);


        Val vCon = eval.clamp(vt.mul(g_vel).sub(vf.mul(g_feed)), mdv).mul(g_con);

        eval.asOut("right", right).asOut("left", left).asOut("mid", mid).asOut("vcon", vCon)
                .asOut("right_b", right_b).asOut("left_b", left_b).asOut("mid_b", mid_b);

        return eval.evaluate().buildContext();
    }

}
