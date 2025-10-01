package com.verr1.controlcraft.foundation.cimulink.standalone.projects;

import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.Circuit;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.CircuitDebugger;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Evaluator;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Val;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.cluster.QuaternionVal;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.cluster.Vector3Val;
import org.joml.Quaterniond;
import org.joml.Quaterniondc;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.Map;

public class WarThunderFlight {

    public static void testLoop(){
        Evaluator eval = new Evaluator();
        Val xn = eval.newVal("xn");
        Val a = eval.newVal("a");
        Val xn_1 = xn.add(a);

        eval.asOut("xn_1", xn_1);
        eval.asLoop("xn_1", "xn");

        Circuit test = eval.evaluate().build("circuit");

        CircuitDebugger dbg = new CircuitDebugger(test);
        test.input("a", 1);

        dbg.printConnections();
        dbg.trackAllOut();
        dbg.trackWithPeriod(1, 1, 10);
        dbg.printTracked();

    }

    public static Evaluator lerpRoll(){
        Evaluator eval = new Evaluator();

        Val level_roll = eval.newVal("lRoll");
        Val aggressive_roll = eval.newVal("aRoll");
        Val angle = eval.newVal("angle");
        Val tol = eval.newVal("tol");

        Val zero = eval.newVal(0.0);
        Val one = eval.newVal(1.0);
        Val inverse = eval.inverseLerp(zero, tol, angle);
        Val con = eval.lerp(level_roll, aggressive_roll, eval.clamp(inverse, zero, one));

        eval.asOut("Roll", con);
        return eval;
    }

    public static Evaluator levelRollCalc(){
        Evaluator eval = new Evaluator();
        QuaternionVal qc = eval.newQuaternion("cQx", "cQy", "cQz", "cQw");
        Val g_lvl = eval.newVal("G_lvl");
        Val zero = eval.newVal(0);
        Val one = eval.newVal(1);

        Vector3Val left = eval.transform(qc, new Vector3Val(one, zero, zero, eval));
        Val leftY = left.y();
        Val levelRoll = leftY.mul(g_lvl);

        eval.asOut("lRoll", levelRoll);
        return eval;
    }

    public static Evaluator linear(){
        Evaluator eval = new Evaluator();
        Vector3Val lerpView = eval.newVector3("lvx", "lvy", "lvz");

        Val g_yaw = eval.newVal("g_yaw");
        Val g_pitch = eval.newVal("g_pitch");
        Val g_roll = eval.newVal("g_roll");

        Val yaw = lerpView.x().mul(g_yaw);
        Val pitch = lerpView.y().mul(g_pitch);
        Val aggressive_roll = lerpView.x().mul(g_roll);

        eval.asOut("Yaw", yaw).asOut("Pitch", pitch).asOut("aRoll", aggressive_roll);
        return eval;
    }

    public static Evaluator sel(){
        Evaluator eval = new Evaluator();

        Val aYaw = eval.newVal("autoYaw");
        Val aPitch = eval.newVal("autoPitch");
        Val aRoll = eval.newVal("autoRoll");

        Val kYaw = eval.newVal("keyYaw");
        Val kPitch = eval.newVal("keyPitch");
        Val kRoll = eval.newVal("keyRoll");

        Val gKYaw = eval.newVal("gk_yaw");
        Val gKPitch = eval.newVal("gk_pitch");
        Val gKRoll = eval.newVal("gk_roll");

        Val tol = eval.newVal("tol");

        Val cYaw = eval.orElse(kYaw.abs().greaterThan(tol), kYaw.mul(gKYaw), aYaw);
        Val cPitch = eval.orElse(kPitch.abs().greaterThan(tol), kPitch.mul(gKPitch), aPitch);
        Val cRoll = eval.orElse(kRoll.abs().greaterThan(tol), kRoll.mul(gKRoll), aRoll);

        eval.asOut("Yaw", cYaw).asOut("Pitch", cPitch).asOut("Roll", cRoll);

        return eval;
    }

    public static void testSel(){
        Circuit sel = sel().evaluate().build("sel");
        CircuitDebugger dbg = new CircuitDebugger(sel);

        sel.input("autoYaw", 0.5);
        sel.input("autoPitch", 0.3);
        sel.input("autoRoll", 0.1);
        sel.input("keyYaw", 0.6);
        sel.input("keyPitch", 0.4);
        sel.input("keyRoll", 0.2);
        sel.input("gk_yaw", 2.0);
        sel.input("gk_pitch", 2.0);
        sel.input("gk_roll", 2.0);
        sel.input("tol", 4);

        dbg.track(sel.__out("Yaw"), sel.__out("Pitch"), sel.__out("Roll"));
        dbg.trackWithPeriod(1, 1, 1);
    }

    public static Evaluator lerp(){
        Evaluator eval = new Evaluator();

        Val rate = eval.newVal("rate");
        Vector3Val view = eval.newVector3("cvx", "cvy", "cvz");
        QuaternionVal lerp = eval.newQuaternion("lerpN_x", "lerpN_y", "lerpN_z", "lerpN_w");
        QuaternionVal curr = eval.newQuaternion("cQx", "cQy", "cQz", "cQw");

        Val zero = eval.newVal(0.0);
        Val one = eval.newVal(1.0);
        Val nOne = eval.newVal(-1.0);

        Vector3Val up = new Vector3Val(zero, one, zero, eval);
        Vector3Val back = new Vector3Val(zero, zero, nOne, eval);

        QuaternionVal safeLerp = lerp.ifZeroThenUnit(zero, one);
        QuaternionVal targetQuat = eval.lookAlong(view, up).conj();
        QuaternionVal nextLerp = eval.slerp(safeLerp, targetQuat, rate);

        Vector3Val lerpView = curr.conj().transform(safeLerp.transform(back));

        eval
                .asOut("lerpV_x", lerpView.x())
                .asOut("lerpV_y", lerpView.y())
                .asOut("lerpV_z", lerpView.z());

        eval
                .asOut("lerpN_1_x", nextLerp.x())
                .asOut("lerpN_1_y", nextLerp.y())
                .asOut("lerpN_1_z", nextLerp.z())
                .asOut("lerpN_1_w", nextLerp.w());
        eval
                .asLoop("lerpN_1_x", "lerpN_x")
                .asLoop("lerpN_1_y", "lerpN_y")
                .asLoop("lerpN_1_z", "lerpN_z")
                .asLoop("lerpN_1_w", "lerpN_w");

        return eval;
    }

    public static CircuitNbt Sel(){
        Evaluator eval = new Evaluator();

        Val pitch_k = eval.newVal("pitch_k");
        Val yaw_k = eval.newVal("yaw_k");
        Val roll_k = eval.newVal("roll_k");

        Val pitch_v = eval.newVal("pitch_v");
        Val yaw_v = eval.newVal("yaw_v");
        Val roll_v = eval.newVal("roll_v");

        Val sel = eval.newVal("sel");

        Val cond = sel.greaterThan(0.5);

        Val pitch_c = eval.orElse(cond, pitch_v, pitch_k);
        Val yaw_c = eval.orElse(cond, yaw_v, yaw_k);
        Val roll_c = eval.orElse(cond, roll_v, roll_k);

        eval.asOut("pitch_c", pitch_c).asOut("yaw_c", yaw_c).asOut("roll_c", roll_c);

        return eval.evaluate().buildContext();
    }


    public static CircuitNbt create(){
        Evaluator eval = new Evaluator();
        eval.defineSubmodule("lerpView", lerp());
        eval.defineSubmodule("levelRoll", levelRollCalc());
        eval.defineSubmodule("linearIn", linear());
        eval.defineSubmodule("lerpRoll", lerpRoll());
        eval.defineSubmodule("sel", sel());

        Vector3Val view = eval.newVector3("viewX", "viewY", "viewZ");
        QuaternionVal quaternion = eval.newQuaternion("cQx", "cQy", "cQz", "cQw");
        Val G_lvl = eval.newVal("g_lvl");
        Val roll_tol = eval.newVal("roll_tol");

        Val G_aYaw = eval.newVal("g_aYaw");
        Val G_aPitch = eval.newVal("g_aPitch");
        Val G_aRoll = eval.newVal("g_aRoll");

        Val G_kYaw = eval.newVal("g_kYaw");
        Val G_kPitch = eval.newVal("g_kPitch");
        Val G_kRoll = eval.newVal("g_kRoll");

        Val kYaw = eval.newVal("kYaw");
        Val kPitch = eval.newVal("kPitch");
        Val kRoll = eval.newVal("kRoll");

        Val G_cYaw = eval.newVal("g_cYaw");
        Val G_cPitch = eval.newVal("g_cPitch");
        Val G_cRoll = eval.newVal("g_cRoll");

        Val key_tol = eval.newVal("key_tol");
        Val rate = eval.newVal("rate");

        Val zero = eval.newVal(0);
        Val one = eval.newVal(1);

        Map<String, Val> lerpViewXYZ = eval.invoke("lerpView", Map.of(
                "cvx", view.x(),
                "cvy", view.y(),
                "cvz", view.z(),
                "cQx", quaternion.x(),
                "cQy", quaternion.y(),
                "cQz", quaternion.z(),
                "cQw", quaternion.w(),
                "rate", rate
        ));

        Val lvx = lerpViewXYZ.get("lerpV_x");
        Val lvy = lerpViewXYZ.get("lerpV_y");
        Val lvz = lerpViewXYZ.get("lerpV_z");

        Map<String, Val> levelRollXYZ = eval.invoke("levelRoll", Map.of(
                "cQx", quaternion.x(),
                "cQy", quaternion.y(),
                "cQz", quaternion.z(),
                "cQw", quaternion.w(),
                "G_lvl", G_lvl
        ));

        Val levelRoll = levelRollXYZ.get("lRoll");

        Map<String, Val> linearYawPitchRoll = eval.invoke("linearIn", Map.of(
                "lvx", lvx,
                "lvy", lvy,
                "g_yaw", G_aYaw,
                "g_pitch", G_aPitch,
                "g_roll", G_aRoll
        ));

        Val aYaw = linearYawPitchRoll.get("Yaw");
        Val aPitch = linearYawPitchRoll.get("Pitch");
        Val aggressiveRoll = linearYawPitchRoll.get("aRoll");

        Val angle = eval.angle(new Vector3Val(lvx, lvy, lvz, eval), new Vector3Val(zero, zero, one, eval));

        Map<String, Val> lerpRoll = eval.invoke("lerpRoll", Map.of(
                "lRoll", levelRoll,
                "aRoll", aggressiveRoll,
                "angle", angle,
                "tol", roll_tol
        ));

        Val aRoll = lerpRoll.get("Roll");

        Map<String, Val> controlYawPitchRoll = eval.invoke("sel", Map.of(
                "autoYaw", aYaw,
                "autoPitch", aPitch,
                "autoRoll", aRoll,
                "keyYaw", kYaw,
                "keyPitch", kPitch,
                "keyRoll", kRoll,
                "gk_yaw", G_kYaw,
                "gk_pitch", G_kPitch,
                "gk_roll", G_kRoll,
                "tol", key_tol
        ));

        Val controlYaw = controlYawPitchRoll.get("Yaw");
        Val controlPitch = controlYawPitchRoll.get("Pitch");
        Val controlRoll = controlYawPitchRoll.get("Roll");

        Val finalYaw = controlYaw.mul(G_cYaw);
        Val finalPitch = controlPitch.mul(G_cPitch);
        Val finalRoll = controlRoll.mul(G_cRoll);

        eval.asOut("cYaw", finalYaw).asOut("cPitch", finalPitch).asOut("cRoll", finalRoll);

        return eval.evaluate().buildContext();

    }

    public static CircuitNbt flight(){
        Evaluator eval = new Evaluator();
        Val pitch = eval.newVal("pitch");
        Val yaw = eval.newVal("yaw");
        Val roll = eval.newVal("roll");
        Val vt = eval.newVal("vel");
        Val vf = eval.newVal("velFeed");

        Val g_vel = eval.newVal("g_vel");
        Val g_feed = eval.newVal("g_vfeed");
        Val g_con = eval.newVal("g_vcon");

        Val nOne = eval.newVal(-1);
        Val one = eval.newVal(1);

        Val g_pitch = eval.newVal("g_pitch");
        Val g_yaw = eval.newVal("g_yaw");
        Val g_roll = eval.newVal("g_roll");

        Val g_right = eval.newVal("g_right");
        Val g_left = eval.newVal("g_left");
        Val g_mid = eval.newVal("g_mid");

        Val gained_pitch = eval.clamp(pitch, nOne, one).mul(g_pitch);
        Val gained_yaw = eval.clamp(yaw, nOne, one).mul(g_yaw);
        Val gained_roll = eval.clamp(roll, nOne, one).mul(g_roll);

        Val right = gained_pitch.add(gained_roll).mul(g_right);
        Val left = gained_pitch.sub(gained_roll).mul(g_left);
        Val mid = gained_yaw.mul(g_mid);

        Val vCon = (vt.mul(g_vel).sub(vf.mul(g_feed))).mul(g_con);

        eval.asOut("right", right).asOut("left", left).asOut("mid", mid).asOut("vcon", vCon);

        return eval.evaluate().buildContext();
    }

    public static CircuitNbt flight2(){
        Evaluator eval = new Evaluator();
        Val pitch = eval.newVal("pitch");
        Val yaw = eval.newVal("yaw");
        Val roll = eval.newVal("roll");
        Val vt = eval.newVal("vel");
        Val vf = eval.newVal("velFeed");

        Val g_vel = eval.newVal("g_vel");
        Val g_feed = eval.newVal("g_vfeed");
        Val g_con = eval.newVal("g_vcon");

        Val nOne = eval.newVal(-1);
        Val one = eval.newVal(1);

        Val g_pitch = eval.newVal("g_pitch");
        Val g_yaw = eval.newVal("g_yaw");
        Val g_roll = eval.newVal("g_roll");

        Val g_right = eval.newVal("g_right");
        Val g_left = eval.newVal("g_left");
        Val g_mid = eval.newVal("g_mid");

        Val gained_pitch = pitch.mul(g_pitch);
        Val gained_yaw = yaw.mul(g_yaw);
        Val gained_roll = roll.mul(g_roll);

        Val right = gained_pitch.add(gained_roll).mul(g_right);
        Val left = gained_pitch.sub(gained_roll).mul(g_left);
        Val mid = gained_yaw.mul(g_mid);

        Val vCon = (vt.mul(g_vel).sub(vf.mul(g_feed))).mul(g_con);

        eval.asOut("right", right).asOut("left", left).asOut("mid", mid).asOut("vcon", vCon);

        return eval.evaluate().buildContext();
    }


    public static CircuitNbt flight3(){
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

        Val vCon = (vt.mul(g_vel).sub(vf.mul(g_feed))).mul(g_con);

        eval.asOut("right", right).asOut("left", left).asOut("mid", mid).asOut("vcon", vCon)
                .asOut("right_b", right_b).asOut("left_b", left_b).asOut("mid_b", mid_b);

        return eval.evaluate().buildContext();
    }

    public static void testLerpRoll(){
        Circuit lerpRoll = lerpRoll().evaluate().build("lerpRoll");
        CircuitDebugger dbg = new CircuitDebugger(lerpRoll);
        lerpRoll.input("lRoll", 0.5);
        lerpRoll.input("aRoll", 1.2);
        lerpRoll.input("angle", 0.1);
        lerpRoll.input("tol", 1);
        dbg.track(lerpRoll.__out("Roll"));
        dbg.trackWithPeriod(1, 1, 1);
    }

    public static void testLevelRoll(){
        Circuit levelRoll = levelRollCalc().evaluate().build("levelRoll");
        CircuitDebugger dbg = new CircuitDebugger(levelRoll);

        Quaterniondc q = new Quaterniond(0.5, 1, 0, 0).normalize();
        double g = 2.1;

        levelRoll.input("cQx", q.x()).input("cQy", q.y()).input("cQz", q.z()).input("cQw", q.w());
        levelRoll.input("G_lvl", g);
        dbg.track(levelRoll.__out("lRoll"));
        dbg.trackWithPeriod(1, 1, 1);

        Vector3dc left = q.transform(new Vector3d(1, 0, 0));
        double c = g * left.y();

        System.out.println("Expected Level Roll: " + c);

    }

    public static void testLinear(){
        Circuit linear = linear().evaluate().build("linear");
        CircuitDebugger dbg = new CircuitDebugger(linear);

        Vector3dc in = new Vector3d(0.5, 0.5, 1);
        Vector3dc ypr = new Vector3d(1.2, 0.8, 0.5);

        linear.input("lvx", in.x()).input("lvy", in.y());
        linear.input("g_yaw", ypr.x()).input("g_pitch", ypr.y()).input("g_roll", ypr.z());

        dbg.track(linear.__out("Yaw"), linear.__out("Pitch"), linear.__out("aRoll"));
        dbg.trackWithPeriod(1, 1, 1);

        System.out.println("Expected: " + in.x() * ypr.x() + " " + in.y() * ypr.y() + " " + in.x() * ypr.z());

    }

    public static void testSlerpLoop(){
        Evaluator eval = new Evaluator();
        Val zero = eval.newVal(0.0);
        Val one = eval.newVal(1.0);

        QuaternionVal qt = eval.newQuaternion("xt", "yt", "zt", "wt");
        QuaternionVal qts = qt.ifZeroThenUnit(zero, one);

        QuaternionVal qv = eval.newQuaternion("x", "y", "z", "w");
        QuaternionVal qvs = qv.ifZeroThenUnit(zero, one);

        QuaternionVal lerpQ = eval.slerp(qvs, qts, eval.newVal(1 - Math.exp(-10 * 0.01667)));

        eval.asOut("xn", lerpQ.x()).asOut("yn", lerpQ.y()).asOut("zn", lerpQ.z()).asOut("wn", lerpQ.w());
        eval.asLoop("xn", "x").asLoop("yn", "y").asLoop("zn", "z").asLoop("wn", "w");


        Circuit c = eval.evaluate().build("safe");

        CircuitDebugger dbg = new CircuitDebugger(c);
        // dbg.printConnections();
        c.input("xt", 1).input("yt", 0).input("zt", 0).input("wt", 0);
        dbg.track(c.__out("xn"), c.__out("yn"), c.__out("zn"), c.__out("wn"));
        // dbg.track(qts.x().port(), qts.y().port(), qts.z().port(), qts.w().port());
        // dbg.track(qv.x().port(), qv.y().port(), qv.z().port(), qv.w().port());
        dbg.trackWithPeriod(1, 1, 15);

    }

    public static void testSlerp(){
        Evaluator eval = new Evaluator();
        Val zero = eval.newVal(0.0);
        Val one = eval.newVal(1.0);

        QuaternionVal qt = eval.newQuaternion("xt", "yt", "zt", "wt");
        QuaternionVal qv = eval.newQuaternion("x", "y", "z", "w");

        QuaternionVal lerpQ = eval.slerp(qv, qt, eval.newVal(1 - Math.exp(-5 * 0.01667)));

        eval.asOut("xo", lerpQ.x()).asOut("yo", lerpQ.y()).asOut("zo", lerpQ.z()).asOut("wo", lerpQ.z());
        Circuit c = eval.evaluate().build("safe");

        CircuitDebugger dbg = new CircuitDebugger(c);
        c.input("xt", 1).input("yt", 0).input("zt", 0).input("wt", 0);
        c.input("x", 0).input("y", 1).input("z", 0).input("w", 0);
        dbg.track(c.__out(0), c.__out(1), c.__out(2), c.__out(3));
        dbg.trackWithPeriod(1, 1, 1);
        System.out.println(new Quaterniond(0, 1, 0, 0).slerp(new Quaterniond(1, 0, 0, 0), 0.2));
    }

    public static void testQ(){
        Evaluator eval = new Evaluator();
        QuaternionVal qv = eval.newQuaternion("x", "y", "z", "w");
        Val zero = eval.newVal(0.0);
        Val one = eval.newVal(1.0);
        QuaternionVal sqv = qv.ifZeroThenUnit(zero, one);
        eval.asOut("xo", sqv.x()).asOut("yo", sqv.y()).asOut("zo", sqv.z()).asOut("wo", sqv.w());
        Circuit c = eval.evaluate().build("safe");

        CircuitDebugger dbg = new CircuitDebugger(c);
        c.input("x", 0.707).input("y", 0.707).input("z", 0).input("w", 0);
        dbg.track(c.__out(0), c.__out(1), c.__out(2), c.__out(3));
        dbg.trackWithPeriod(1, 1, 1);

    }

    public static void testLerp(){
        Circuit lerp = lerp().evaluate().build("lerp");
        CircuitDebugger dbg = new CircuitDebugger(lerp);

        Vector3dc in = new Vector3d(1, 2, 3);

        lerp.input("rate", 0.9);
        lerp.input("cvx", in.x()).input("cvy", in.y()).input("cvz", in.z());
        lerp.input("cQw", 1);

        // dbg.track(lerp.__out("lerpNdb_1_x"), lerp.__out("lerpNdb_1_y"), lerp.__out("lerpNdb_1_z"), lerp.__out("lerpNdb_1_w"));
        dbg.track(lerp.__out("lerpV_x"), lerp.__out("lerpV_y"), lerp.__out("lerpV_z"));// dbg.trackOut();
        dbg.trackWithPeriod(1, 1, 10);
        System.out.println(in.normalize(new Vector3d()));
        // System.out.println(new Quaterniond().lookAlong(new Vector3d(0.12, 0.5, 1), new Vector3d(0, 1, 0)));

    }

}
