package com.verr1.controlcraft.unstable.pathing;

public interface LerpPath<C> {

    int segments();

    C lerp(int seg);

}
