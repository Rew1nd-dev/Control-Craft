package com.verr1.controlcraft.foundation.cimulink.standalone.projects;

import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.Circuit;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.CircuitDebugger;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Evaluator;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Val;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.cluster.QuaternionVal;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.cluster.Vector3Val;
import com.verr1.controlcraft.utils.VSAccessUtils;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.Map;

public class AimPredict {


    public static Evaluator aim(){
        Evaluator eval = new Evaluator();
        Vector3Val pt = eval.newVector3("ptx", "pty", "ptz"); // target position relative to base position, in world coordinate
        Vector3Val vt = eval.newVector3("vtx", "vty", "vtz"); // target velocity in world coordinate
        Val bv = eval.newVal("bv"); // bullet speed
        Val zero = eval.newVal(0);
        Val inf = eval.newVal(1e7);
        Val vt_mag_2 = vt.lengthSquare();
        Val a = vt_mag_2.sub(bv.mul(bv));
        Val b = vt.dot(pt).mul(2);
        Val c = pt.lengthSquare();
        Val discriminant = b.mul(b).sub(a.mul(c).mul(4));
        Val invalid = discriminant.lessThan(zero);
        Val safeDiscriminant = eval.max(discriminant, zero);
        Val sqrtDisc = eval.sqrt(safeDiscriminant);
        Val t1 = b.neg().add(sqrtDisc).div(a.mul(2));
        Val t2 = b.neg().sub(sqrtDisc).div(a.mul(2));
        Val t1_gt_0  = t1.greaterThan(zero);
        Val t2_le_0  = t2.lessThan(zero);
        Val t2_gt_0  = t2.greaterThan(zero);
        Val t1_le_t2 = t1.lessThan(t2);
        Val t_hit_0 = eval.orElse(
                t1_gt_0.and(t2_le_0.or(t1_le_t2)),
                t1,
                eval.orElse(
                        t2_gt_0,
                        t2,
                        inf
                )
        );
        Val t_hit_1 = eval.orElse(
                discriminant.lessThan(zero),
                zero,
                eval.orElse(
                        t_hit_0.equal(inf),
                        zero,
                        t_hit_0
                )
        );
        Vector3Val p_aim = pt.add(vt.scale(t_hit_1));
        eval.asOut("ax", p_aim.x()).asOut("ay", p_aim.y()).asOut("az", p_aim.z());

        return eval;
    }

    public static void testAim(){
        Circuit aim = aim().evaluate().build("aim");
        CircuitDebugger db = new CircuitDebugger(aim);
        db.trackOut();
        Vector3dc ptx = new Vector3d(100, 100, 100);
        Vector3dc vtx = new Vector3d(10, 0, 0);
        double bv = 50;
        aim.input("ptx", ptx.x()).input("pty", ptx.y()).input("ptz", ptx.z());
        aim.input("vtx", vtx.x()).input("vty", vtx.y()).input("vtz", vtx.z());
        aim.input("bv", bv);

        Vector3dc aimTrue = VSAccessUtils.aimPredict(ptx, vtx, new Vector3d(0, 0, 0), bv);
        System.out.println("True Aim: " + (aimTrue == null ? "null" : aimTrue));

        db.trackWithPeriod(1, 1, 1);


    }

    public static CircuitNbt create(){
        Evaluator eval = new Evaluator();
        eval.defineSubmodule("aim", aim());

        Val n1 = eval.newVal(-1);

        Vector3Val pc = eval.newVector3("cx", "cy", "cz"); // base position in world coordinate
        Vector3Val oc = eval.newVector3("ox", "oy", "oz"); // base offset position in ship coordinate
        Vector3Val pt = eval.newVector3("ptx", "pty", "ptz"); // target position in world coordinate
        Vector3Val vt = eval.newVector3("vtx", "vty", "vtz"); // target velocity in world coordinate
        QuaternionVal qc = eval.newQuaternion("qcx", "qcy", "qcz", "qcw"); // base orientation
        Val bv = eval.newVal("bv"); // bullet speed

        Map<String, Val> aim_out = eval.invoke("aim", Map.of(
                "ptx", pt.x().sub(pc.x()),
                "pty", pt.y().sub(pc.y()),
                "ptz", pt.z().sub(pc.z()),
                "vtx", vt.x(),
                "vty", vt.y(),
                "vtz", vt.z(),
                "bv", bv
        ));

        Vector3Val aim_wc = new Vector3Val(
                aim_out.get("ax"),
                aim_out.get("ay"),
                aim_out.get("az"),
                eval
        );

        Vector3Val aim_sc = qc.conj().transform(aim_wc).add(oc.scale(n1)).normalize();

        Val yaw = eval.atan(aim_sc.x(), aim_sc.z());
        Val pitch = eval.asin(aim_sc.y());

        eval.asOut("yaw", yaw).asOut("pitch", pitch);

        return eval.evaluate().buildContext();
    }

}
