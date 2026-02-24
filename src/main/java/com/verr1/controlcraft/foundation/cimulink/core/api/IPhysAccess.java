package com.verr1.controlcraft.foundation.cimulink.core.api;

import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies;
import org.joml.Quaterniond;
import org.joml.Quaterniondc;
import org.joml.Vector3d;
import org.joml.Vector3dc;

public interface IPhysAccess {

    IPhysAccess EMPTY = new IPhysAccess() {
        @Override
        public Quaterniondc quaternionToWorld() {
            return new Quaterniond();
        }

        @Override
        public Vector3dc position() {
            return new Vector3d();
        }

        @Override
        public Vector3dc velocity() {
            return new Vector3d();
        }

        @Override
        public Vector3dc angularVelocity() {
            return new Vector3d();
        }

        @Override
        public double mass() {
            return 0;
        }

        @Override
        public double inertia() {
            return 0;
        }


    };

    Quaterniondc quaternionToWorld();

    Vector3dc position();

    Vector3dc velocity();

    Vector3dc angularVelocity();

    double mass();

    double inertia();

    static IPhysAccess of(OnShipBlockEntity be){
        return new IPhysAccess() {
            @Override
            public Quaterniondc quaternionToWorld() {
                return be.readSelf().quaternion();
            }

            @Override
            public Vector3dc position() {
                Vector3d p_sc = ValkyrienSkies.set(new Vector3d(), be.getBlockPos().getCenter());
                return be.readSelf().s2wTransform().transformPosition(p_sc);
            }

            @Override
            public Vector3dc velocity() {
                return be.readSelf().velocity();
            }

            @Override
            public Vector3dc angularVelocity() {
                return be.readSelf().omega();
            }

            @Override
            public double mass() {
                return be.readSelf().mass();
            }

            @Override
            public double inertia() {
                return be.readSelf().inertiaTensor().m00();
            }
        };
    }

}
