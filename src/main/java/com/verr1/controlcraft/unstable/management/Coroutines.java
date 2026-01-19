package com.verr1.controlcraft.unstable.management;

import java.util.List;

public class Coroutines {


    public static ImmediateRoutine immediate(Runnable task){
        return new ImmediateRoutine(task);
    }

    public static SerialRoutine chained(CoroutineBase... tasks){
        return new SerialRoutine(List.of(tasks));
    }


}
