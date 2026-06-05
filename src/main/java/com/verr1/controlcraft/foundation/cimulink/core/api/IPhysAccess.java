package com.verr1.controlcraft.foundation.cimulink.core.api;

import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import org.joml.*;
import org.valkyrienskies.core.api.ships.Ship;
import org.valkyrienskies.mod.api.ValkyrienSkies;

import java.util.Optional;

public interface IPhysAccess {
    Matrix4dc EMPTY_M4 = new Matrix4d();
    Vector3dc EMPTY_V3 = new Vector3d();

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
        public Vector3dc positionCenterModel() {
            return new Vector3d();
        }

        @Override
        public Vector3dc positionCenter() {
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

    Vector3dc positionCenterModel();

    Vector3dc positionCenter();

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
            public Vector3dc positionCenterModel() {
                return be.positionCenterModel();
            }

            @Override
            public Vector3dc positionCenter() {
                return be.positionCenter();
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

    static IPhysAccess ofMainThread(OnShipBlockEntity be){
        return new IPhysAccess() {
            @Override
            public Quaterniondc quaternionToWorld() {
                return Optional.ofNullable(be.getShipOn()).map(v -> v.getTransform().getShipToWorldRotation()).orElse(new Quaterniond());
            }

            @Override
            public Vector3dc position() {
                Vector3d p_sc = ValkyrienSkies.set(new Vector3d(), be.getBlockPos().getCenter());
                Matrix4dc trans = Optional
                    .ofNullable(be.getShipOn())
                    .map(Ship::getShipToWorld)
                    .orElse(EMPTY_M4);
                return trans.transformPosition(p_sc);
            }

            @Override
            public Vector3dc positionCenterModel() {
                return be.positionCenterModel();
            }

            @Override
            public Vector3dc positionCenter() {
                return be.positionCenter();
            }

            @Override
            public Vector3dc velocity() {
                return Optional
                    .ofNullable(be.getShipOn())
                    .map(Ship::getVelocity)
                    .orElse(EMPTY_V3);
            }

            @Override
            public Vector3dc angularVelocity() {
                return Optional
                    .ofNullable(be.getShipOn())
                    .map(Ship::getOmega)
                    .orElse(EMPTY_V3);
            }

            @Override
            public double mass() {
                return 1;
            }

            @Override
            public double inertia() {
                return 1;
            }
        };
    }

}