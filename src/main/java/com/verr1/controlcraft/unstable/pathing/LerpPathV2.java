package com.verr1.controlcraft.unstable.pathing;

import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.List;

public interface LerpPathV2<C> {
    LerpPathV2<Vector3dc> EMPTY_3D = new LerpPathV2<>() {
        @Override
        public double length() {
            return 0;
        }

        @Override
        public Vector3dc point(double length) {
            return new Vector3d(0, 0, 0);
        }
    };

    double length();

    C point(double length);

    default C end(){
        return point(length());
    }

    static<C> LerpPathV2<C> fromList(List<C> wayPoints){
        return new LerpPathV2<>() {
            @Override
            public double length() {
                return wayPoints.size() - 1;
            }

            @Override
            public C point(double length) {
                int index = (int) Math.floor(length);
                if (index < 0 || index >= wayPoints.size()) {
                    throw new IndexOutOfBoundsException("Index out of bounds for length: " + length);
                }
                return wayPoints.get(index);
            }
        };
    }

}
