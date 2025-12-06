package com.verr1.controlcraft.foundation.cimulink.standalone.projects;

import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Evaluator;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Val;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.cluster.QuaternionVal;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.cluster.Vector3Val;

public class CCRP {

    public static Evaluator autoDrop(){
        Evaluator eval = new Evaluator();
        Val one = eval.newVal(1);
        Val zero = eval.newVal(0);

        Vector3Val hit = eval.newVector3("hx", "hy", "hz");
        Vector3Val pos = eval.newVector3("cx", "cy", "cz");
        Vector3Val off = eval.newVector3("ox", "oy", "oz");
        QuaternionVal qat = eval.newQuaternion("qx", "qy", "qz", "qw");

        Vector3Val pin = eval.newVector3("px", "py", "pz");
        Val tol = eval.newVal("tol");

        Val g_yaw = eval.newVal("g_yaw");
        Val g_pitch = eval.newVal("g_pitch");
        Val g_roll = eval.newVal("g_roll");

        Vector3Val front = new Vector3Val(zero, zero, one, eval);
        Vector3Val left = new Vector3Val(one, zero, zero, eval);

        Vector3Val front_wc = qat.transform(front);
        Vector3Val left_wc = qat.transform(left);

        Val pitch = eval.asin(front_wc.y());
        Val roll = eval.asin(left_wc.y());


        Vector3Val off_wc = qat.transform(off);
        Val drop = hit.add(pos.add(off_wc)).sub(pin).length().lessThan(tol);


        Vector3Val dir = qat.conj().transform(pin.sub(pos).normalize());
        Val yaw = eval.atan(dir.x(), dir.z());
        Val yaw_con = yaw.mul(g_yaw);
        Val pitch_con = pitch.mul(g_pitch);
        Val roll_con = roll.mul(g_roll);


        eval.asOut("trigger", drop).asOut("cYaw", yaw_con).asOut("cPitch", pitch_con).asOut("cRoll", roll_con);

        return eval;
    }

    public static Evaluator autoAim(){
        Evaluator eval = new Evaluator();

        Vector3Val hit = eval.newVector3("hx", "hy", "hz");
        Vector3Val pos = eval.newVector3("cx", "cy", "cz");
        Vector3Val off = eval.newVector3("ox", "oy", "oz");
        QuaternionVal qat = eval.newQuaternion("qx", "qy", "qz", "qw");
        Vector3Val pin = eval.newVector3("px", "py", "pz");
        Vector3Val off_wc = qat.transform(off);
        Vector3Val delta = hit.add(pos.add(off_wc)).sub(pin);

        eval.asOut("dX", delta.x()).asOut("dY", delta.y()).asOut("dZ", delta.z());

        return eval;
    }

    public static Evaluator ccrpCon(){
        Evaluator eval = new Evaluator();
        Vector3Val delta = eval.newVector3("dX", "dY", "dZ");
        Vector3Val pin = eval.newVector3("px", "py", "pz");
        Vector3Val pos = eval.newVector3("cx", "cy", "cz");
        Vector3Val off_sc = eval.newVector3("ox", "oy", "oz");
        QuaternionVal qat = eval.newQuaternion("qx", "qy", "qz", "qw");


        Vector3Val delta_pin_sc = qat.conj().transform(pin.sub(pos)).sub(off_sc);
        Vector3Val delta_sc = qat.conj().transform(delta);
        Val dot = delta_sc.dot(delta_pin_sc).div(delta_pin_sc.lengthSquare());
        Val m_pitch = eval.newVal("m_pitch");

        Val yaw = eval.atan(delta_pin_sc.x(), delta_pin_sc.z());
        Val pitch = eval.clamp(dot, m_pitch);

        eval.asOut("yaw", yaw).asOut("pitch", pitch);

        return eval;
    }

    public static Evaluator fireCon(){
        Evaluator eval = new Evaluator();

        Val wingTrigger = eval.newVal("wing_trigger");
        Val wingTriggerPosEdge = eval.newPositiveTrigger(wingTrigger, "_@");

        Val lastCycle = eval.newVal("last_cycle");
        Val cycleInc = lastCycle.add(wingTriggerPosEdge);
        Val cycle = eval.orElse(cycleInc.greaterThan(5), eval.newVal(0), cycleInc);
        eval.asOut("cycle", cycle);
        eval.asLoop("cycle", "last_cycle");

        Val[] triggerRocketPosEdge = new Val[6];
        for(int i = 0; i < 6; i++){
            triggerRocketPosEdge[i] = eval.newPositiveTrigger(cycle.equal(eval.newVal(i)), "_@" + i);
        }
        Val[] wingRocketPosEdge = new Val[6];
        for(int i = 0; i < 6; i++){
            wingRocketPosEdge[i] = eval.newPositiveTrigger(eval.newVal("wing_rocket_" + i), "_@" + i);
        }
        Val[] rocketOut = new Val[6];
        for(int i = 0; i < 6; i++){
            rocketOut[i] = wingRocketPosEdge[i].or(triggerRocketPosEdge[i]);
            eval.asOut("rocket_" + i, rocketOut[i]);
        }



        Val auto = eval.newVal("auto_bomb");
        Val autoBomb = eval.newPositiveTrigger(eval.newVal("auto_bomb_trigger"), "_@");
        Val leadBomb = eval.newPositiveTrigger(eval.newVal("lead_bomb"), "_@");
        Val wingBomb = eval.newPositiveTrigger(eval.newVal("wing_bomb"), "_@");

        Val bombOut = eval.orElse(auto.greaterThan(0), autoBomb, leadBomb.or(wingBomb));
        eval.asOut("bomb", bombOut);

        return eval;
    }

//    public static Evaluator testLoop(){
//        Evaluator eval = new Evaluator();
//        Val in = eval.newVal("testIn");
//
//    }

    public static Evaluator testLoop(){
        Evaluator eval = new Evaluator();
//        Vector3Val vIn = eval.newVector3("x", "y", "z");
//        Val inT = in.mul(1);
//        Val trigger = eval.newPositiveTrigger(inT, "_");
//        eval.asOut("trigger", trigger);
//
//        Vector3Val lastVal = eval.newVector3("_x1", "_y1", "_z1");
//
//        Val edge = vIn.x().and(lastVal.x().not());
//
//        eval.asOut("edge", edge);
//        eval.asLoop("x", "_x1");

        Val in = eval.newVal("in");
        Val _in = eval.newVal("_in");

        eval.asOut("li", in.mul(1));
        eval.asLoop("li", "_in");

        Val edge = in.and(_in.not());
        eval.asOut("edge", edge);

        return eval;
    }

    public static Evaluator sel3(){
        Evaluator eval = new Evaluator();
        Val s0 = eval.newVal("s0");
        Val s1 = eval.newVal("s1");

        Val pitch_view = eval.newVal("vPitch");
        Val yaw_view = eval.newVal("vYaw");
        Val roll_view = eval.newVal("vRoll");

        Val pitch_manual = eval.newVal("mPitch");
        Val yaw_manual = eval.newVal("mYaw");
        Val roll_manual = eval.newVal("mRoll");

        Val pitch_auto = eval.newVal("aPitch");
        Val yaw_auto = eval.newVal("aYaw");
        Val roll_auto = eval.newVal("aRoll");

        Val pitch = eval.orElse(s0, pitch_auto, eval.orElse(s1, pitch_view, pitch_manual));
        Val yaw = eval.orElse(s0, yaw_auto, eval.orElse(s1, yaw_view, yaw_manual));
        Val roll = eval.orElse(s0, roll_auto, eval.orElse(s1, roll_view, roll_manual));

        eval.asOut("oPitch", pitch).asOut("oYaw", yaw).asOut("oRoll", roll);

        return eval;
    }

    public static Evaluator cannon8(){
        Evaluator eval = new Evaluator();

        Val fire = eval.newVal("fire");
        Val fireTrigger = eval.newPositiveTrigger(fire, "_@");
        Val lastCycle = eval.newVal("last_cycle");
        Val cycleInc = lastCycle.add(fireTrigger);
        Val cycle = eval.orElse(cycleInc.greaterThan(7), eval.newVal(0), cycleInc);
        eval.asOut("cycle", cycle);
        eval.asLoop("cycle", "last_cycle");

        Val[] fireEdges = new Val[8];
        for(int i = 0; i < 8; i++){
            fireEdges[i] = eval.newPositiveTrigger(cycle.equal(eval.newVal(i)), "_@" + i);
        }

        for(int i = 0; i < 8; i++){
            eval.asOut("cannon_" + i, fireEdges[i]);
        }


        Val pitchInDeg = eval.newVal("pitch_c_deg");
        Val pitchTarRad = eval.newVal("pitch_t_rad");
        Val gain_pitch = eval.newVal("gain_pitch");

        Val pitchErr = pitchTarRad.sub(pitchInDeg.mul(Math.PI / 180));
        Val pitchCon = pitchErr.mul(gain_pitch);
        eval.asOut("pitch_con", pitchCon);

        return eval;
    }

}
