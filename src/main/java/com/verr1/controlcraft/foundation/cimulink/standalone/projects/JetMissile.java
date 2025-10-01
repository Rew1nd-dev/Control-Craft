package com.verr1.controlcraft.foundation.cimulink.standalone.projects;

import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Evaluator;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Val;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.cluster.QuaternionVal;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.cluster.Vector3Val;

import java.util.List;

public class JetMissile {

    public static Evaluator closeLoopXYZ(){
        Evaluator eval= new Evaluator();

        Val g_mass = eval.newVal("g_mass");
        Val g_vcon = eval.newVal("g_vcon");
        Val g_inertia = eval.newVal("g_inertia");
        Val g_rot_x = eval.newVal("g_rotx");
        Val g_rot_y = eval.newVal("g_roty");
        Val g_drot_x = eval.newVal("g_drotx");
        Val g_drot_y = eval.newVal("g_droty");


        Val yaw = eval.newVal("yaw");
        Val pitch = eval.newVal("pitch");
        Val tvz = eval.newVal("vel");
        Val vz = eval.newVal("velFeed");
        Val wy = eval.newVal("wy");
        Val wx = eval.newVal("wx");

        Val z = g_mass.mul(g_vcon).mul(tvz.sub(vz));
        Val x = g_inertia.mul(g_rot_x.mul(yaw).add(g_drot_x.mul(wy)));
        Val y = g_inertia.mul(g_rot_y.mul(pitch).add(g_drot_y.mul(wx)));


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

    public static Evaluator computeYP(){
        Evaluator eval = new Evaluator();


        Vector3Val tar = eval.newVector3("tx", "ty", "tz");
        Vector3Val cur = eval.newVector3("cx", "cy", "cz");
        QuaternionVal q = eval.newQuaternion("qx", "qy", "qz", "qw");

        Vector3Val dir = tar.sub(cur).normalize();

        Vector3Val v_sc = q.conj().transform(dir);

        Val yaw = eval.atan(v_sc.x(), v_sc.z());
        Val pitch = eval.asin(v_sc.y());

        eval.asOut("yaw", yaw).asOut("pitch", pitch);

        return eval;
    }


}
