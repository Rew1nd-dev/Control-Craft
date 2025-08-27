package com.verr1.controlcraft.unstable.ai.api;

import org.joml.Vector3dc;

public interface IAnchorContext {

    boolean noGroundTarget();

    Vector3dc getPosition();

    double cruiseRadius();

    Vector3dc getGroundTarget();

    double height();

    static IAnchorContext of(IAttackerContext context){
        return new IAnchorContext() {
            @Override
            public boolean noGroundTarget() {
                return context.noGroundTarget();
            }

            @Override
            public Vector3dc getPosition() {
                return context.getPosition();
            }

            @Override
            public double cruiseRadius() {
                return context.cruiseRadius();
            }

            @Override
            public Vector3dc getGroundTarget() {
                return context.getGroundTarget();
            }

            @Override
            public double height() {
                return context.height();
            }
        };
    }

    static IAnchorContext of(IFighterJetContext context){

        return new IAnchorContext() {
            @Override
            public boolean noGroundTarget() {
                return context.noTarget();
            }

            @Override
            public Vector3dc getPosition() {
                return context.getPosition();
            }

            @Override
            public double cruiseRadius() {
                return context.cruiseRadius();
            }

            @Override
            public Vector3dc getGroundTarget() {
                return context.below();
            }

            @Override
            public double height() {
                return context.height();
            }
        };
    }

}
