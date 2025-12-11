package com.verr1.controlcraft.foundation.cimulink.core.api;

import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import org.joml.Quaterniond;
import org.joml.Quaterniondc;
import org.joml.Vector3d;
import org.joml.Vector3dc;

public interface IPhysWorldAccess {

    IPhysWorldAccess EMPTY = new IPhysWorldAccess() {
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
    };

    Quaterniondc quaternionToWorld();

    Vector3dc position();

    Vector3dc velocity();

    Vector3dc angularVelocity();

    static IPhysWorldAccess of(OnShipBlockEntity be){
        return new IPhysWorldAccess() {
            @Override
            public Quaterniondc quaternionToWorld() {
                return be.readSelf().quaternion();
            }

            @Override
            public Vector3dc position() {
                return be.readSelf().position();
            }

            @Override
            public Vector3dc velocity() {
                return be.readSelf().velocity();
            }

            @Override
            public Vector3dc angularVelocity() {
                return be.readSelf().omega();
            }
        };
    }

}
