package com.verr1.controlcraft.foundation.cimulink.standalone.eval;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.records.ComponentPortName;

import java.util.Optional;

public class Val {

    private final ValTransit parent;
    private final Evaluator evaluator;
    private final String actualPortName;

    Val(ValTransit parent, String actualPortName) {
        this.parent = parent;
        this.evaluator = null;
        this.actualPortName = actualPortName;
    }

    Val(Evaluator parent, String inputName) {
        this.parent = null;
        this.evaluator = parent;
        this.actualPortName = inputName;
    }

    public ValTransit parent() {
        return parent;
    }

    public String portName() {
        return actualPortName;
    }

    public String componentName(){
        return Optional.ofNullable(parent()).map(ValTransit::component).map(NamedComponent::name).orElse("@null");
    }

    public ComponentPortName port(){
        return new ComponentPortName(componentName(), portName());
    }

    private Evaluator eval(){
        return Optional.ofNullable(evaluator).orElseGet(() -> parent().evaluator());
    }

    public Val[] join(Val... other){
        Val[] joined = new Val[other.length + 1];
        joined[0] = this;
        System.arraycopy(other, 0, joined, 1, other.length);
        return joined;
    }

    public Val add(Val... other){
        return eval().add(join(other));
    }

    public Val sub(Val other){
        return add(1, -1, other);
    }

    public Val add(double coeffSelf, double coeffOther, Val other){
        return eval().add(
                coeffSelf, this,
                coeffOther, other
        );
    }

    public Val mul(double constant){
        return eval().mul(constant, this);
    }

    public Val mul(Val... other){
        return eval().mul(join(other));
    }

    public Val sqrt(){
        return eval().sqrt(this);
    }

    public Val div(Val other) {
        return eval().div(this, other);
    }

    public Val power(Val exponent) {
        return eval().power(this, exponent);
    }

    public Val abs() {
        return eval().abs(this);
    }

    public Val sin() {
        return eval().sin(this);
    }

    public Val cos() {
        return eval().cos(this);
    }

    public Val tan() {
        return eval().tan(this);
    }

    public Val asin() {
        return eval().asin(this);
    }

    public Val acos() {
        return eval().acos(this);
    }

    public Val max(Val... other) {
        return eval().max(join(other));
    }

    public Val min(Val... other) {
        return eval().min(join(other));
    }

    public Val and(Val... other) {
        return eval().and(join(other));
    }

    public Val or(Val... other) {
        return eval().or(join(other));
    }

    public Val not() {
        return eval().not(this);
    }

    public Val xor(Val... other) {
        return eval().xor(join(other));
    }

    public Val nand(Val... other) {
        return eval().nand(join(other));
    }

    public Val greaterThan(Val other) {
        return eval().greaterThan(this, other);
    }

    public Val lessThan(Val other) {
        return eval().lessThan(this, other);
    }

    public Val equal(Val other) {
        return eval().equal(this, other);
    }

    public Val greaterThan(double other) {
        return eval().greaterThan(this, eval().newVal(other));
    }

    public Val lessThan(double other) {
        return eval().lessThan(this, eval().newVal(other));
    }

    public Val equal(double other) {
        return eval().equal(this, eval().newVal(other));
    }

    public Val neg(){
        return eval().neg(this);
    }

    public Val orElse(Val trueVal, Val falseVal){
        return eval().orElse(this, trueVal, falseVal);
    }

}
