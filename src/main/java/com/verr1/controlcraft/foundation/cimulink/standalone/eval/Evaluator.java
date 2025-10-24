package com.verr1.controlcraft.foundation.cimulink.standalone.eval;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.analog.Shifter;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.CircuitConstructor;
import com.verr1.controlcraft.foundation.cimulink.core.components.vectors.LookAlong;
import com.verr1.controlcraft.foundation.cimulink.core.records.ComponentPortName;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.FuncOperations;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.LogicOperations;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.analog.Compare;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.analog.Constant;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.analog.LinearAdd;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.analog.OrElse;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.cluster.QuaternionVal;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.cluster.Vector3Val;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.submodule.Module;
import com.verr1.controlcraft.foundation.cimulink.standalone.eval.operation.vector.*;
import com.verr1.controlcraft.foundation.cimulink.standalone.projects.Eval;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

public class Evaluator {

    private final Map<String, InputVal> inputs = new HashMap<>();
    private final Map<String, Val> outs = new HashMap<>();

    private final Map<String, String> regs = new HashMap<>();
    private final Map<String, String> revRegs = new HashMap<>();

    private final Map<String, Submodule> submodules = new HashMap<>();

    public Val newVal(String name){
        if(inputs.containsKey(name)){
            throw new IllegalArgumentException("Input with name '" + name + "' already exists.");
        }
        return inputs.computeIfAbsent(name, k -> new InputVal(k, this));
    }

    public Val newPositiveTrigger(Val val, String prefix){
        String inName = prefix + "_i_" + val.portName();
        String outName = prefix + "_o_" + val.portName();
        Val lastVal = newVal(inName);
        asOut(outName, val.mul(1));
        asLoop(outName, inName);
        return val.and(lastVal.not());
    }

    public Val newNegativeTrigger(Val val, String prefix){
        String inName = prefix + "_i_" + val.portName();
        String outName = prefix + "_o_" + val.portName();
        Val lastVal = newVal(inName);
        asOut(outName, val.mul(1));
        asLoop(outName, inName);
        return val.not().and(lastVal);
    }

    public Vector3Val newVector3(String name){
        return new Vector3Val(
                newVal(name + "_x"),
                newVal(name + "_y"),
                newVal(name + "_z"),
                this
        );
    }

    public Vector3Val newVector3(String x, String y, String z){
        return new Vector3Val(
                newVal(x),
                newVal(y),
                newVal(z),
                this
        );
    }

    public QuaternionVal newQuaternion(String x, String y, String z, String w){
        return new QuaternionVal(
                newVal(x),
                newVal(y),
                newVal(z),
                newVal(w),
                this
        );
    }

    public QuaternionVal newQuaternion(String name){
        return new QuaternionVal(
                newVal(name + "_qx"),
                newVal(name + "_qy"),
                newVal(name + "_qz"),
                newVal(name + "_qw"),
                this
        );
    }


    public Val newVal(double constant){
        return new Constant(constant, this).out();
    }

    public Evaluator defineSubmodule(String name, Evaluator evaluator){
        if(submodules.containsKey(name)){
            throw new IllegalArgumentException("Submodule with name '" + name + "' already exists.");
        }
        submodules.put(name, new Submodule(evaluator));
        return this;
    }

    public Evaluator asLoop(String outName, String inName){
        if(revRegs.containsKey(inName)){
            throw new IllegalArgumentException("Input with name '" + inName + "' is already used as a feedback input.");
        }
        if(regs.containsKey(outName)){
            throw new IllegalArgumentException("Output with name '" + outName + "' is already used as a feedback output.");
        }
        regs.put(outName, inName);
        revRegs.put(inName, outName);
        return this;
    }

    public Evaluator asLoop(Val outVal, Val inVal){
        return asLoop(outVal.portName(), inVal.portName());
    }

    public Evaluator asLoop(String outName, Val inVal){
        return asLoop(outName, inVal.portName());
    }

    public Evaluator asOut(String name, Val out){
        if(outs.containsKey(name)){
            throw new IllegalArgumentException("Output with name '" + name + "' already exists.");
        }
        outs.put(name, out);
        return this;
    }

    public Evaluator asOut(Val out){
        asOut(out.portName(), out);
        return this;
    }

    public CircuitConstructor evaluate(){
        Queue<Val> q = new ArrayDeque<>(outs.values());
        Set<ValTransit> transits = new HashSet<>();
        while (!q.isEmpty()){
            Val val = q.poll();
            ValTransit transit = val.parent();
            if(transit == null)continue;
            transits.add(transit);
            transit.ins().values().forEach(q::offer);
        }

        Map<String, Shifter> allRegs = new HashMap<>();
        regs.forEach((out, in) -> allRegs.put(out, new Shifter(0, 1)));

        CircuitConstructor constructor = new CircuitConstructor();
        constructor.addComponent(transits.stream().map(ValTransit::component).toArray(NamedComponent[]::new)); // name is converted during addComponent() process
        constructor.addComponent(allRegs.values().toArray(Shifter[]::new));

        outs.forEach((name, outVal) -> {
            ComponentPortName outComponentPortName = new ComponentPortName(
                    outVal.componentName(),
                    outVal.portName()
            );
            if(regs.containsKey(name)){
                Shifter reg = allRegs.get(name);
                constructor.connect(outComponentPortName, reg.__in(0));
            }else{
                constructor.defineOutput(name, outComponentPortName);
            }
        });
        transits.forEach(valTransit -> {
            valTransit.ins().forEach((thisPortName, outputPort) -> {
                String outputComponentName = outputPort.componentName();
                String outputPortName = outputPort.portName();
                String inputComponentName = valTransit.component().name();
                String inputPortName = thisPortName;
                ComponentPortName inComponentPortName = new ComponentPortName(inputComponentName, inputPortName);
                if(outputComponentName.equals("@null")){

                    if(revRegs.containsKey(outputPortName)){ // outputPortName --> circuit input name
                        Shifter reg = allRegs.get(revRegs.get(outputPortName));
                        constructor.connect(reg.__out(0), inComponentPortName);
                    } else{
                        constructor.defineInput(outputPortName, inComponentPortName);
                    }


                }else {
                    constructor.connect(
                            new ComponentPortName(outputComponentName, outputPortName),
                            new ComponentPortName(inputComponentName, inputPortName)
                    );
                }


            });
        });



        return constructor;
    }

    public Map<String, Val> invoke(String subModuleName, Map<String, Val> args){
        if(!submodules.containsKey(subModuleName)){
            throw new IllegalArgumentException("Submodule with name '" + subModuleName + "' does not exist.");
        }
        return new Module(args, submodules.get(subModuleName).build(), this).outs();
    }

    public Val add(Val... vals){
        double[] coeffs = new double[vals.length];
        Arrays.fill(coeffs, 1.0);
        return new LinearAdd(coeffs, vals, this).out();
    }

    public Val add(double ca, Val a, double cb, Val b){
        LinearAdd fma = new LinearAdd(ca, a, cb, b, this);
        return fma.out();
    }

    public Val mul(double ca, Val a){
        LinearAdd fma = new LinearAdd(new double[]{ca}, new Val[]{a}, this);
        return fma.out();
    }

    public Val mul(Val... vals){
        ValTransit mul = FuncOperations.MUL.create(this, vals);
        return mul.out();
    }

    public Val sqrt(Val x){
        return power(x, newVal(0.5));
    }


    public Val div(Val a, Val b){
        ValTransit div = FuncOperations.DIV.create(this, a, b);
        return div.out();
    }

    public Val greaterThan(Val a, Val b) {
        Compare cmp = new Compare(a, b, this);
        return cmp.ge();
    }

    public Val lessThan(Val a, Val b) {
        Compare cmp = new Compare(a, b, this);
        return cmp.le();
    }

    public Val equal(Val a, Val b) {
        Compare cmp = new Compare(a, b, this);
        return cmp.eq();
    }

    public List<Val> compare(Val a, Val b){
        Compare cmp = new Compare(a, b, this);
        return List.of(cmp.ge(), cmp.le(), cmp.eq());
    }

    public Val orElse(Val condition, Val trueVal, Val falseVal){
        OrElse orElse = new OrElse(condition, trueVal, falseVal, this);
        return orElse.out();
    }

    public Val neg(Val a){
        return mul(-1, a);
    }

    public Val asin(Val x){
        return FuncOperations.ASIN.create(this, x).out();
    }

    public Val acos(Val x){
        return FuncOperations.ACOS.create(this, x).out();
    }

    public Val power(Val a, Val b) {
        return FuncOperations.POWER.create(this, a, b).out();
    }

    public Val abs(Val a) {
        return FuncOperations.ABS.create(this, a).out();
    }

    public Val sin(Val a) {
        return FuncOperations.SIN.create(this, a).out();
    }

    public Val cos(Val a) {
        return FuncOperations.COS.create(this, a).out();
    }

    public Val tan(Val a) {
        return FuncOperations.TAN.create(this, a).out();
    }

    public Val atan(Val y, Val x) {
        return FuncOperations.ATAN.create(this, y, x).out();
    }

    public Val max(Val... vals) {
        return FuncOperations.MAX.create(this, vals).out();
    }

    public Val min(Val... vals) {
        return FuncOperations.MIN.create(this, vals).out();
    }

    // Logic operations
    public Val and(Val... vals) {
        return LogicOperations.AND.create(this, vals).out();
    }

    public Val or(Val... vals) {
        return LogicOperations.OR.create(this, vals).out();
    }

    public Val xor(Val... vals) {
        return LogicOperations.XOR.create(this, vals).out();
    }

    public Val nand(Val... vals) {
        return LogicOperations.NAND.create(this, vals).out();
    }

    public Val not(Val a) {
        return LogicOperations.NOT.create(this, a).out();
    }

    public Val fma(Val a, Val b, Val c){
        return add(a, mul(b, c));
    }

    public Val mag(Val... vals){
        Val[] square = Arrays.stream(vals)
                .map(v -> mul(v, v))
                .toArray(Val[]::new);
        return sqrt(add(square));
    }

    public List<Val> norm(Val... vals){
        Val mag = mag(vals);
        return Arrays.stream(vals)
                .map(v -> div(v, mag))
                .toList();
    }

    public Vector3Val transform(QuaternionVal q, Vector3Val v){
        var qvt = new QVTransform(
                q.x(), q.y(), q.z(), q.w(),
                v.x(), v.y(), v.z(),
                this
        );
        return new Vector3Val(
                qvt.x(), qvt.y(), qvt.z(),
                this
        );
    }

    public Val clamp(Val x, Val min, Val max){
        return min(max, max(min, x));
    }

    public Val clamp(Val x, Val max){
        return min(max, max(max.neg(), x));
    }

    public Val lerp(Val start, Val end, Val rate){
        return start.add(end.sub(start).mul(rate));
    }

    public Val inverseLerp(Val start, Val end, Val val){
        return val.sub(start).div(end.sub(start));
    }

    public QuaternionVal conjugate(QuaternionVal q){
        return new QuaternionVal(neg(q.x()), neg(q.y()), neg(q.z()), q.w(), this);
    }

    public QuaternionVal slerp(QuaternionVal start, QuaternionVal end, Val rate){
        var qs = new QSlerp(
                start.x(), start.y(), start.z(), start.w(),
                end.x(), end.y(), end.z(), end.w(),
                rate,
                this
        );
        return new QuaternionVal(
                qs.x(), qs.y(), qs.z(), qs.w(),
                this
        );
    }

    public QuaternionVal lookAlong(Vector3Val dir, Vector3Val up){
        var qt = new QLookAlong(
                dir.x(), dir.y(), dir.z(),
                up.x(), up.y(), up.z(),
                this
        );

        return new QuaternionVal(
                qt.x(), qt.y(), qt.z(), qt.w(),
                this
        );
    }

    public Val dot(Vector3Val v0, Vector3Val v1){
        return new VDot(v0.x(), v0.y(), v0.z(), v1.x(), v1.y(), v1.z(), this).out();
    }

    public Vector3Val cross(Vector3Val v0, Vector3Val v1){
        var c = new VCross(v0.x(), v0.y(), v0.z(), v1.x(), v1.y(), v1.z(), this);

        return new Vector3Val(c.x(), c.y(), c.z(), this);
    }

    public Val angle(Vector3Val v0, Vector3Val v1){
        return acos(dot(v0, v1));
    }
}
