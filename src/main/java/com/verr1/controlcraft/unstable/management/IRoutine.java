package com.verr1.controlcraft.unstable.management;

public interface IRoutine {

    RoutineStatus resume(int steps);

    int count();

    boolean closed();

    void abort();



}
