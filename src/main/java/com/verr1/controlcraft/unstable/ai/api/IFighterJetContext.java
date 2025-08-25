package com.verr1.controlcraft.unstable.ai.api;

import org.joml.Vector3dc;

public interface IFighterJetContext extends IAirContext{

    double shootTolerance();

    void fireAt(Vector3dc target);

    Vector3dc getTargetPosition();

    Vector3dc getTargetVelocity();

    default void kill(){};

    default boolean noTarget(){return false;}

    default Vector3dc getCruiseTarget(){
        return getTargetPosition();
    }

}
