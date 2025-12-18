package com.verr1.controlcraft.utils;

import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.function.Function;

public class LazyField<T> {
    public static final Function<Double, EqualPredicate<Double>> DOUBLE_COMP = th -> (t1, t2) -> Math.abs(t1 - t2) < th;
    public static final Function<Double, EqualPredicate<Float>> FLOAT_COMP = th -> (t1, t2) -> Math.abs(t1 - t2) < th;
    public static final Function<Double, EqualPredicate<Vector3dc>> VEC_DIR_COMP = th -> (t1, t2) -> t1.sub(t2, new Vector3d()).lengthSquared() < th;

    T data;
    final EqualPredicate<T> changed;
    boolean isDirty = false;

    public LazyField(T initial, EqualPredicate<T> changed) {
        this.changed = changed;
        this.data = initial;
    }

    public boolean pollDirty(){
        if(isDirty){
            isDirty = false;
            return true;
        }
        return false;
    }

    public T get(){
        return data;
    }

    public void set(T newData){
        if(newData == null || changed.isEqual(data, newData)){
            return;
        }
        data = newData;
        isDirty = true;
    }

    public interface EqualPredicate<T>{

        boolean isEqual(T t1, T t2);
    }
}
