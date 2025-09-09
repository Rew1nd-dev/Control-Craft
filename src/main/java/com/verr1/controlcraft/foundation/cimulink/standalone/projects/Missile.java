package com.verr1.controlcraft.foundation.cimulink.standalone.projects;

import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.Circuit;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.CircuitDebugger;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Evaluator;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Val;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.cluster.QuaternionVal;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.cluster.Vector3Val;

import java.util.HashMap;
import java.util.Map;

public class Missile {

    public static Evaluator deltaCoordinate(){
        Evaluator eval = new Evaluator();

        Val tx = eval.newVal("tx");
        Val ty = eval.newVal("ty");
        Val tz = eval.newVal("tz");

        Val cx = eval.newVal("cx");
        Val cy = eval.newVal("cy");
        Val cz = eval.newVal("cz");

        Vector3Val dir = new Vector3Val(
                tx.sub(cx),
                ty.sub(cy),
                tz.sub(cz),
                eval
        ).normalize();

        eval.asOut("dx", dir.x()).asOut("dy", dir.y()).asOut("dz", dir.z());

        return eval;
    }


    public static Evaluator computeYawPitch(){
        Evaluator eval = new Evaluator();

        Vector3Val v_wc = eval.newVector3("dx", "dy", "dz").normalize(); // view
        QuaternionVal q = eval.newQuaternion("qx", "qy", "qz", "qw");
        Vector3Val v_sc = q.conj().transform(v_wc);

        Val yaw = eval.atan(v_sc.x(), v_sc.z());
        Val pitch = eval.asin(v_sc.y());

        eval.asOut("yaw", yaw).asOut("pitch", pitch);

        return eval;
    }

    public static Evaluator computeYawPitchPredict(){
        Evaluator eval = new Evaluator();
        eval.defineSubmodule("aim", AimPredict.aim());

        Vector3Val p_wc = eval.newVector3("dx", "dy", "dz");

        QuaternionVal q = eval.newQuaternion("qx", "qy", "qz", "qw");
        Val ts = eval.newVal("ts_rev"); // 1 / ts
        Val bv = eval.newVal("bv"); // flying speed

        Vector3Val p_wc_prev = eval.newVector3("pdx", "pdy", "pdz"); // previous view

        Vector3Val p_sc = q.conj().transform(p_wc);
        Vector3Val dv_wc = p_wc.sub(p_wc_prev).scale(ts);

        Vector3Val dv = q.conj().transform(dv_wc);

        Map<String, Val> aim_out = eval.invoke("aim", Map.of(
                "ptx", p_sc.x(),
                "pty", p_sc.y(),
                "ptz", p_sc.z(),
                "vtx", dv.x(),
                "vty", dv.y(),
                "vtz", dv.z(),
                "bv", bv
        ));

        Vector3Val aim = new Vector3Val(
                aim_out.get("ax"),
                aim_out.get("ay"),
                aim_out.get("az"),
                eval
        ).normalize();


        Val yaw = eval.atan(aim.x(), aim.z());
        Val pitch = eval.asin(aim.y());

        eval.asOut("cdx", p_wc.x().mul(1))
            .asOut("cdy", p_wc.y().mul(1))
            .asOut("cdz", p_wc.z().mul(1));

        eval.asLoop("cdx", "pdx").asLoop("cdy", "pdy").asLoop("cdz", "pdz");

        eval.asOut("yaw", yaw).asOut("pitch", pitch);

        return eval;
    }

    public static Evaluator proportionalGuiding(){
        Evaluator eval = new Evaluator();

        Vector3Val tw = eval.newVector3("twx_wc", "twy_wc", "twz_wc").normalize();
        Vector3Val cw = eval.newVector3("cwx_wc", "cwy_wc", "cwz_wc").normalize();

        QuaternionVal q = eval.newQuaternion("qx", "qy", "qz", "qw");



        Vector3Val d_wc = tw.add(cw.scale(eval.newVal(-1))).normalize();

        Vector3Val d_sc = q.conj().transform(d_wc);



        eval.asOut("dwy", d_sc.y()).asOut("dwx", d_sc.x()).asOut("dwz", d_sc.z());
        return eval;
    }


    public static void testYP(){

        Circuit cyp = computeYawPitch().evaluate().build("yp");
        CircuitDebugger db = new CircuitDebugger(cyp);
        db.trackOut();
        cyp.input("dz", 1).input("dy", 1).input("qw", 1);

        db.trackWithPeriod(1, 1, 1);

    }

    public static Evaluator control(){
        Evaluator eval = new Evaluator();

        Val yaw = eval.newVal("yaw");
        Val pitch = eval.newVal("pitch");

        Val g_yaw = eval.newVal("g_yaw");
        Val g_pitch = eval.newVal("g_pitch");

        Val g_com = eval.newVal("g_com");

        Val g_left = eval.newVal("g_left").mul(g_com);
        Val g_left_b = eval.newVal("g_left_b").mul(g_com);

        Val g_right = eval.newVal("g_right").mul(g_com);
        Val g_right_b = eval.newVal("g_right_b").mul(g_com);

        Val g_up = eval.newVal("g_up").mul(g_com);
        Val g_up_b = eval.newVal("g_up_b").mul(g_com);

        Val g_down = eval.newVal("g_down").mul(g_com);
        Val g_down_b = eval.newVal("g_down_b").mul(g_com);

        Val one = eval.newVal(1);
        Val nOne = eval.newVal(-1);

        Val gained_pitch = eval.clamp(pitch.mul(g_pitch), nOne, one);
        Val gained_yaw = eval.clamp(yaw.mul(g_yaw), nOne, one);

        Val left = gained_pitch.mul(g_left);
        Val right = gained_pitch.mul(g_right);
        Val up = gained_yaw.mul(g_up);
        Val down = gained_yaw.mul(g_down);

        Val left_b = gained_pitch.mul(g_left_b);
        Val right_b = gained_pitch.mul(g_right_b);
        Val up_b = gained_yaw.mul(g_up_b);
        Val down_b = gained_yaw.mul(g_down_b);

        eval.asOut("left", left).asOut("right", right)
            .asOut("up", up).asOut("down", down)

            .asOut("left_b", left_b).asOut("right_b", right_b)
            .asOut("up_b", up_b).asOut("down_b", down_b);

        return eval;
    }

    public static CircuitNbt create(){
        Evaluator eval = new Evaluator();
        eval.defineSubmodule("cyp", computeYawPitch());
        eval.defineSubmodule("ctrl", control());

        Val maxDv = eval.newVal("Mdv");
        Val vel_feed = eval.newVal("vel_feed");
        Val vel = eval.newVal("vel");
        Val g_vel = eval.newVal("g__vel");
        Val g_vel_feed = eval.newVal("g__vel_f");
        Val p_vel = eval.newVal("p_vel");
        Val dv = (vel.mul(g_vel).sub(vel_feed.mul(g_vel_feed)));

        Val d_vel = eval.clamp(dv, maxDv.neg(), maxDv).mul(p_vel);
        Val g_com = eval.newVal("g_com");

        Vector3Val v_wc = eval.newVector3("dx", "dy", "dz").normalize();
        QuaternionVal q = eval.newQuaternion("qx", "qy", "qz", "qw");

        Map<String, Val> cyp_out = eval.invoke("cyp", Map.of(
                "dx", v_wc.x(),
                "dy", v_wc.y(),
                "dz", v_wc.z(),
                "qx", q.x(),
                "qy", q.y(),
                "qz", q.z(),
                "qw", q.w()
        ));

        Val g_yaw = eval.newVal("g__yaw");
        Val g_pitch = eval.newVal("g__pitch");

        Val g_left = eval.newVal("g_left");
        Val g_left_b = eval.newVal("g_left_b");

        Val g_right = eval.newVal("g_right");
        Val g_right_b = eval.newVal("g_right_b");

        Val g_up = eval.newVal("g_up");
        Val g_up_b = eval.newVal("g_up_b");

        Val g_down = eval.newVal("g_down");
        Val g_down_b = eval.newVal("g_down_b");


        Map<String, Val> args = new HashMap<>();
        args.putAll(Map.of("yaw", cyp_out.get("yaw"),
                "pitch", cyp_out.get("pitch"),
                "g_yaw", g_yaw,
                "g_pitch", g_pitch,
                "g_left", g_left,
                "g_left_b", g_left_b));

        args.putAll(Map.of(
                "g_right", g_right,
                "g_right_b", g_right_b,
                "g_up", g_up,
                "g_up_b", g_up_b,
                "g_down", g_down,
                "g_down_b", g_down_b,
                "g_com", g_com
        ));

        Map<String, Val> ctrl_out = eval.invoke("ctrl", args);

        eval.asOut("left", ctrl_out.get("left"))
            .asOut("right", ctrl_out.get("right"))
            .asOut("up", ctrl_out.get("up"))
            .asOut("down", ctrl_out.get("down"))

            .asOut("left_b", ctrl_out.get("left_b"))
            .asOut("right_b", ctrl_out.get("right_b"))
            .asOut("up_b", ctrl_out.get("up_b"))
            .asOut("down_b", ctrl_out.get("down_b"))

            .asOut("vel_con", d_vel);

        return eval.evaluate().buildContext();
    }


    public static CircuitNbt createPredict(){
        Evaluator eval = new Evaluator();
        eval.defineSubmodule("cyp", computeYawPitchPredict());
        eval.defineSubmodule("ctrl", control());

        Val maxDv = eval.newVal("Mdv");
        Val vel_feed = eval.newVal("vel_feed");
        Val vel = eval.newVal("vel");
        Val g_vel = eval.newVal("g__vel");

        Val ts = eval.newVal("ts_rev"); // 1 / ts

        Val g_vel_feed = eval.newVal("g__vel_f");
        Val p_vel = eval.newVal("p_vel");
        Val dv = (vel.mul(g_vel).sub(vel_feed.mul(g_vel_feed)));

        Val d_vel = eval.clamp(dv, maxDv.neg(), maxDv).mul(p_vel);


        Vector3Val v_wc = eval.newVector3("dx", "dy", "dz").normalize();
        QuaternionVal q = eval.newQuaternion("qx", "qy", "qz", "qw");

        Map<String, Val> cyp_out = eval.invoke("cyp", Map.of(
                "dx", v_wc.x(),
                "dy", v_wc.y(),
                "dz", v_wc.z(),
                "qx", q.x(),
                "qy", q.y(),
                "qz", q.z(),
                "qw", q.w(),
                "ts_rev", ts,
                "bv", vel_feed
        ));

        Val g_yaw = eval.newVal("g__yaw");
        Val g_pitch = eval.newVal("g__pitch");

        Val g_left = eval.newVal("g_left");
        Val g_left_b = eval.newVal("g_left_b");

        Val g_right = eval.newVal("g_right");
        Val g_right_b = eval.newVal("g_right_b");

        Val g_up = eval.newVal("g_up");
        Val g_up_b = eval.newVal("g_up_b");

        Val g_down = eval.newVal("g_down");
        Val g_down_b = eval.newVal("g_down_b");

        Val g_com = eval.newVal("g_com");

        Map<String, Val> args = new HashMap<>();
        args.putAll(Map.of(
                "yaw", cyp_out.get("yaw"),
                "pitch", cyp_out.get("pitch"),
                "g_yaw", g_yaw,
                "g_pitch", g_pitch,
                "g_left", g_left,
                "g_left_b", g_left_b,
                "g_com", g_com
        ));

        args.putAll(Map.of(
                "g_right", g_right,
                "g_right_b", g_right_b,
                "g_up", g_up,
                "g_up_b", g_up_b,
                "g_down", g_down,
                "g_down_b", g_down_b
        ));

        Map<String, Val> ctrl_out = eval.invoke("ctrl", args);

        eval.asOut("left", ctrl_out.get("left"))
                .asOut("right", ctrl_out.get("right"))
                .asOut("up", ctrl_out.get("up"))
                .asOut("down", ctrl_out.get("down"))

                .asOut("left_b", ctrl_out.get("left_b"))
                .asOut("right_b", ctrl_out.get("right_b"))
                .asOut("up_b", ctrl_out.get("up_b"))
                .asOut("down_b", ctrl_out.get("down_b"))

                .asOut("vel_con", d_vel);

        return eval.evaluate().buildContext();
    }

}
