package com.verr1.controlcraft.foundation.cimulink.standalone.projects;

import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Evaluator;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Val;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.cluster.Vector3Val;

import java.util.Map;

public class FPV {

    public static Evaluator closeLoop(){
        Evaluator eval = new Evaluator();

        Val p = eval.newVal("g_rot");
        Val d = eval.newVal("g_dRot");

        Val inertia = eval.newVal("g_inertia");


        Vector3Val cw_sc = eval.newVector3("cwx", "cwy", "cwz");
        Vector3Val tw_sc = eval.newVector3("twx", "twy", "twz");

        Vector3Val torque =
                tw_sc.sub(cw_sc).scale(p)
        .sub(
                cw_sc.scale(d)
        )
                .scale(inertia);

        eval.asOut("tqx", torque.x()).asOut("tqy", torque.y()).asOut("tqz", torque.z());

        return eval;
    }

    public static Evaluator decompose(){
        Evaluator eval = new Evaluator();

        Val fy = eval.newVal("fy");

        Val af = eval.newVal("af");
        Val bm = eval.newVal("bm");
        Val r  = eval.newVal("ar");

        Val afr = af.mul(r);

        Val wx = eval.newVal("wx").div(afr);
        Val wy = eval.newVal("wy").div(bm);
        Val wz = eval.newVal("wz").div(afr);

        Val w1 = fy.mul(0.25).add(wx.mul(-0.25)).add(wy.mul( 0.25)).add(wz.mul( 0.25));
        Val w2 = fy.mul(0.25).add(wx.mul( 0.25)).add(wy.mul(-0.25)).add(wz.mul( 0.25));
        Val w3 = fy.mul(0.25).add(wx.mul( 0.25)).add(wy.mul( 0.25)).add(wz.mul(-0.25));
        Val w4 = fy.mul(0.25).add(wx.mul(-0.25)).add(wy.mul(-0.25)).add(wz.mul(-0.25));

        eval.asOut("w1", w1)
            .asOut("w2", w2)
            .asOut("w3", w3)
            .asOut("w4", w4);

        return eval;

    }

    public static Evaluator create(){

        Evaluator eval = new Evaluator();
        eval.defineSubmodule("closeLoop", closeLoop());
        eval.defineSubmodule("decompose", decompose());

        Vector3Val ypr = eval.newVector3("yaw", "pitch", "roll");

        Val throttle = eval.newVal("throttle");

        Val g_yaw = eval.newVal("g_yaw");
        Val g_pitch = eval.newVal("g_pitch");
        Val g_roll = eval.newVal("g_roll");
        Val g_throttle = eval.newVal("g_throttle");

        Val mass = eval.newVal("g_mass");


        Val p = eval.newVal("g_pRot");
        Val d = eval.newVal("g_dRot");

        Val inertia = eval.newVal("g_inertia");

        Val af = eval.newVal("af");
        Val bm = eval.newVal("bm");
        Val r  = eval.newVal("ar");

        Vector3Val cw_sc = eval.newVector3("cwx", "cwy", "cwz");
        Vector3Val tw_sc = new Vector3Val(
                g_pitch.mul(ypr.y()),   // pitch -- x
                g_yaw.mul(ypr.x()),     // yaw -- y
                g_roll.mul(ypr.z()),    // roll -- z
                eval
        );

        Val force = g_throttle.mul(throttle).mul(mass);

        Map<String, Val> torque = eval.invoke("closeLoop", Map.of(
                "g_rot", p,
                "g_dRot", d,
                "g_inertia", inertia,
                "cwx", cw_sc.x(),
                "cwy", cw_sc.y(),
                "cwz", cw_sc.z(),
                "twx", tw_sc.x(),
                "twy", tw_sc.y(),
                "twz", tw_sc.z()
        ));

        Map<String, Val> control = eval.invoke("decompose", Map.of(
                "wx", torque.get("tqx"),
                "wy", torque.get("tqy"),
                "wz", torque.get("tqz"),
                "fy", force,
                "af", af,
                "bm", bm,
                "ar", r
        ));


        eval.asOut("w1", control.get("w1"))
            .asOut("w2", control.get("w2"))
            .asOut("w3", control.get("w3"))
            .asOut("w4", control.get("w4"));

        return eval;

    }


}
