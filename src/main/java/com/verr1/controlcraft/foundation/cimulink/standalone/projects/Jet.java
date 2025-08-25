package com.verr1.controlcraft.foundation.cimulink.standalone.projects;

import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Evaluator;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Val;

import java.util.List;
import java.util.Map;

public class Jet {

    private static Evaluator jet(){
        Evaluator eval = new Evaluator();

        Val x = eval.newVal("x");
        Val y = eval.newVal("y");
        Val z = eval.newVal("z");

        List<Val> normed = eval.norm(x, y, z);
        Val xn = normed.get(0);
        Val yn = normed.get(1);
        Val zn = normed.get(2);

        Val mag = eval.mag(x, y, z);

        Val ah0 = xn.neg().asin();
        Val av0 = yn.neg().div(ah0.cos()).asin();
        Val sign = eval.orElse(z.greaterThan(0), eval.newVal(1), eval.newVal(-1));
        Val av = av0.mul(sign);
        Val ah = ah0.mul(sign);
        Val th = mag.mul(sign.neg());

        eval.asOut("av", av).asOut("ah", ah).asOut("th", th);

        return eval;
    }

    public static Evaluator decomposition(){
        Evaluator eval = new Evaluator();

        Val r1 = eval.newVal("r1");
        Val r0 = eval.newVal("r0");
        Val Tx = eval.newVal("Tx");
        Val Ty = eval.newVal("Ty");
        Val Tz = eval.newVal("Tz");
        Val Fz = eval.newVal("Fz");

        Val Tx_r1 = Tx.div(r1);
        Val Tz_r0 = Tz.div(r0);

        Val ly = eval.add(0.5, Tx_r1, -0.5, Tz_r0);
        Val ry = eval.add(0.5, Tx_r1,  0.5, Tz_r0);

        Val lx_rx = Ty.div(r1).mul(0.5);
        Val lz_rz = Fz.mul(0.5);

        eval.asOut("lx", lx_rx).asOut("ly", ly).asOut("lz", lz_rz)
            .asOut("rx", lx_rx).asOut("ry", ry).asOut("rz", lz_rz);

        return eval;
    }

    public static Evaluator input(){

        Evaluator eval = new Evaluator();

        Val roll = eval.newVal("roll");
        Val pitch = eval.newVal("pitch");
        Val yaw = eval.newVal("yaw");
        Val speed = eval.newVal("speed");

        Val G_roll = eval.newVal("G_roll");
        Val G_pitch = eval.newVal("G_pitch");
        Val G_yaw = eval.newVal("G_yaw");
        Val G_v = eval.newVal("G_v");

        Val constant = eval.newVal(1.5);

        Val twz = G_roll.mul(roll.power(constant));
        Val twy = G_yaw.mul(yaw.power(constant));
        Val twx = G_pitch.mul(pitch.power(constant));
        Val tvz = G_v.mul(speed);

        eval.asOut("twz", twz)
            .asOut("twy", twy)
            .asOut("twx", twx)
            .asOut("tvz", tvz);

        return eval;
    }


    public static Evaluator closeRotLoop(){
        Evaluator eval = new Evaluator();

        Val twx = eval.newVal("twx");
        Val twy = eval.newVal("twy");
        Val twz = eval.newVal("twz");

        Val cwx = eval.newVal("cwx");
        Val cwy = eval.newVal("cwy");
        Val cwz = eval.newVal("cwz");

        Val p = eval.newVal("P");
        Val d = eval.newVal("D");

        Val gc = eval.newVal("Gc");

        Val Gtw = p;
        Val Gcw = p.sub(d);


        Val Tx0 = Gtw.mul(twx).sub(Gcw.mul(cwx));
        Val Ty0 = Gtw.mul(twy).sub(Gcw.mul(cwy));
        Val Tz0 = Gtw.mul(twz).sub(Gcw.mul(cwz));

        Val Tx = Tx0.mul(gc);
        Val Ty = Ty0.mul(gc);
        Val Tz = Tz0.mul(gc);

        eval.asOut("Tx", Tx)
            .asOut("Ty", Ty)
            .asOut("Tz", Tz);

        return eval;
    }

    public static Evaluator closeVelLoop(){
        Evaluator eval = new Evaluator();

        Val cv = eval.newVal("cv");
        Val tv = eval.newVal("tv");

        Val gc = eval.newVal("Gc");

        Val Fz = gc.mul(tv.sub(cv));

        eval.asOut("Fz", Fz);

        return eval;
    }

    public static CircuitNbt create(){
        Evaluator eval = new Evaluator();

        eval.defineSubmodule("attacker", jet())
            .defineSubmodule("decomposition", decomposition())
            .defineSubmodule("input", input())
            .defineSubmodule("closeRotLoop", closeRotLoop())
            .defineSubmodule("closeVelLoop", closeVelLoop());

        Val roll = eval.newVal("roll");
        Val pitch = eval.newVal("pitch");
        Val yaw = eval.newVal("yaw");
        Val speed = eval.newVal("speed");
        Val G_speed = eval.newVal("G_speed");
        Val G_roll = eval.newVal("G_roll");
        Val G_pitch = eval.newVal("G_pitch");
        Val G_yaw = eval.newVal("G_yaw");

        Val cwx = eval.newVal("cwx");
        Val cwy = eval.newVal("cwy");
        Val cwz = eval.newVal("cwz");
        Val cvz = eval.newVal("cvz");

        Val p = eval.newVal("P");
        Val d = eval.newVal("D");
        Val r1 = eval.newVal("r1");
        Val r0 = eval.newVal("r0");
        Val G_rot = eval.newVal("G_rot");
        Val G_vel = eval.newVal("G_mass");

        Map<String, Val> tw_xyz = eval.invoke("input", Map.of(
                "roll", roll,
                "pitch", pitch,
                "yaw", yaw,
                "speed", speed,
                "G_roll", G_roll,
                "G_pitch", G_pitch,
                "G_yaw", G_yaw,
                "G_v", G_speed
        ));

        Map<String, Val> T_xyz = eval.invoke("closeRotLoop", Map.of(
                "twx", tw_xyz.get("twx"),
                "twy", tw_xyz.get("twy"),
                "twz", tw_xyz.get("twz"),
                "cwx", cwx,
                "cwy", cwy,
                "cwz", cwz,
                "P", p,
                "D", d,
                "Gc", G_rot
        ));

        Map<String, Val> F_xyz = eval.invoke("closeVelLoop", Map.of(
                "tv", tw_xyz.get("tvz"),
                "cv", cvz,
                "Gc", G_vel
        ));


        Map<String, Val> rl_xyz = eval.invoke("decomposition", Map.of(
                "r1", r1,
                "r0", r0,
                "Tx", T_xyz.get("Tx"),
                "Ty", T_xyz.get("Ty"),
                "Tz", T_xyz.get("Tz"),
                "Fz", F_xyz.get("Fz")
        ));

        Map<String, Val> out_l = eval.invoke("attacker", Map.of(
                "x", rl_xyz.get("lx"),
                "y", rl_xyz.get("ly"),
                "z", rl_xyz.get("lz")
        ));

        Map<String, Val> out_r = eval.invoke("attacker", Map.of(
                "x", rl_xyz.get("rx"),
                "y", rl_xyz.get("ry"),
                "z", rl_xyz.get("rz")
        ));

        eval.asOut("av_l", out_l.get("av"))
            .asOut("ah_l", out_l.get("ah"))
            .asOut("th_l", out_l.get("th"))
            .asOut("av_r", out_r.get("av"))
            .asOut("ah_r", out_r.get("ah"))
            .asOut("th_r", out_r.get("th"));


        return eval.evaluate().buildContext();
    }

}
