package com.verr1.controlcraft.unstable.ai.api;

import org.joml.Vector3dc;

public interface ICircleContext {

    double height();

    double circleRadius();

    Vector3dc getPosition();

    Vector3dc getVelocity();

    IAirController controller();

    static ICircleContext ofAttacker(IAttackerContext context){
        return new ICircleContext() {
            @Override
            public double height() {
                return context.height();
            }

            @Override
            public double circleRadius() {
                return context.cruiseRadius();
            }

            @Override
            public Vector3dc getPosition() {
                return context.getPosition();
            }

            @Override
            public Vector3dc getVelocity() {
                return context.getVelocity();
            }

            @Override
            public IAirController controller() {
                return context.controller();
            }
        };
    }

    static ICircleContext ofFighter(IFighterJetContext context){
        return new ICircleContext() {
            @Override
            public double height() {
                return context.height();
            }

            @Override
            public double circleRadius() {
                return context.cruiseRadius();
            }

            @Override
            public Vector3dc getPosition() {
                return context.getPosition();
            }

            @Override
            public Vector3dc getVelocity() {
                return context.getVelocity();
            }

            @Override
            public IAirController controller() {
                return context.controller();
            }
        };
    }

}
