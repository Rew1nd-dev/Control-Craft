package com.verr1.controlcraft.foundation.cimulink.standalone.projects;

import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.Circuit;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.CircuitConstructor;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.CircuitDebugger;
import com.verr1.controlcraft.foundation.cimulink.core.records.ComponentPortName;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Evaluator;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.Val;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.List;
import java.util.Map;

public class Eval {


    public static void test(){

        Evaluator eval = new Evaluator();

        Val a = eval.newVal("a");
        Val b = eval.newVal("b");

        Val c = eval.add(a, b);
        Val d = eval.mul(a, b);

        Val e = eval.div(d, c);

        eval.asOut("ab/(a + b)", e).asOut("test", e);

        CircuitConstructor constructor = eval.evaluate();
        Circuit circuit = constructor.build();

        CircuitDebugger db = new CircuitDebugger(circuit);

        circuit.input("a", 1).input("b", 2);

        circuit.cycle();

        db.printOutputs();

    }

    public static void test_1(){

        Evaluator eval = new Evaluator();

        Val a = eval.newVal("a");
        Val b = eval.newVal("b");

        Val sqrt = eval.sqrt(a);
        Val ge = eval.greaterThan(a, eval.newVal(0.5));
        Val le = eval.lessThan(a, eval.newVal(0.5));
        Val eq = eval.equal(a, eval.newVal(0.5));
        Val os = eval.orElse(b, a, sqrt);
        Val ne = eval.neg(a);
        Val asin = eval.asin(a);
        Val acos = eval.acos(a);
        Val atan = eval.atan(a, b);
        Val pow = eval.power(a, b);
        Val abs = eval.abs(a);
        Val sin = eval.sin(a);
        Val cos = eval.cos(a);
        Val tan = eval.tan(a);
        Val min = eval.min(a, b);
        Val max = eval.max(a, b);
        Val and = eval.and(a, b);
        Val or = eval.or(a, b);
        Val xor = eval.xor(a, b);
        Val mag = eval.mag(a, b);


        eval
                .asOut("sqrt(a)", sqrt)
                .asOut("ge(a, 0.5)", ge)
                .asOut("le(a, 0.5)", le)
                .asOut("eq(a, 0.5)", eq)
                .asOut("orElse(b, a, sqrt)", os)
                .asOut("neg(a)", ne)
                .asOut("asin(a)", asin)
                .asOut("acos(a)", acos)
                .asOut("atan(a)", atan)
                .asOut("power(a, b)", pow)
                .asOut("abs(a)", abs)
                .asOut("sin(a)", sin)
                .asOut("cos(a)", cos)
                .asOut("tan(a)", tan)
                .asOut("min(a, b)", min)
                .asOut("max(a, b)", max)
                .asOut("and(a, b)", and)
                .asOut("or(a, b)", or)
                .asOut("xor(a, b)", xor)
                .asOut("mag", mag);

        CircuitConstructor constructor = eval.evaluate();
        Circuit circuit = constructor.build();

        CircuitDebugger db = new CircuitDebugger(circuit);

        circuit.input("a", 0.6).input("b", 2);

        circuit.cycle();

        db.track(circuit.outputs().stream().map(
                o -> new ComponentPortName(circuit.name(), o)
        ).toArray(ComponentPortName[]::new));

        // db.trackAllOut();
        db.trackWithPeriod(1, 1 ,1);

    }
/*
* eval.asOut("xo", xn).asOut("yo", yn).asOut("zo", zn);
        CircuitConstructor constructor = eval.evaluate();
        Circuit circuit = constructor.build();
        CircuitDebugger db = new CircuitDebugger(circuit);
        circuit.input("x", 1).input("y", 2).input("z", 3);
        circuit.cycle();
        db.track(circuit.outputs().stream().map(
                o -> new ComponentPortName(circuit.name(), o)
        ).toArray(ComponentPortName[]::new));
        db.trackWithPeriod(1, 1 ,1);
* */
/*
* CircuitConstructor constructor = eval.evaluate();
        Circuit circuit = constructor.build();
        CircuitDebugger db = new CircuitDebugger(circuit);

        double dx = 1;
        double dy = 2;
        double dz = 3;
        circuit.input("x", dx).input("y", dy).input("z", dz);
        circuit.cycle();
        db.printOutputs();
        System.out.println(testFunc(dx, dy, dz));
* */
    public static Evaluator jet(){
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

    public static void testSubModule(){
        Evaluator jet = jet();
        Evaluator eval = new Evaluator();

        eval.defineSubmodule("attacker", jet);

        Val x0 = eval.newVal("x0");
        Val y0 = eval.newVal("y0");
        Val z0 = eval.newVal("z0");

        Val x1 = eval.newVal("x1");
        Val y1 = eval.newVal("y1");
        Val z1 = eval.newVal("z1");

        Map<String, Val> out_0 = eval.invoke("attacker", Map.of(
                "x", x0,
                "y", y0,
                "z", z0
        ));

        Map<String, Val> out_1 = eval.invoke("attacker", Map.of(
                "x", x1,
                "y", y1,
                "z", z1
        ));

    }

    public static List<Double> testFunc(double x, double y, double z){
        Vector3dc norm = new Vector3d(x, y, z).normalize();
        double xn = norm.x();
        double yn = norm.y();
        double zn = norm.z();
        double mag = Math.sqrt(x * x + y * y + z * z);
        double ah0 = Math.asin(-xn);
        double av0 = Math.asin(-yn / Math.cos(ah0));
        double sign = z > 0 ? 1 : -1;
        double av = av0 * sign;
        double ah = ah0 * sign;
        double th = mag * -sign;
        return List.of(av, ah, th);
    }

}
