package com.verr1.controlcraft.unstable.ai.core;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

public class Blackboard {
    private final Map<Address<?>, Object> data = new HashMap<>();


    public Blackboard() {
    }

    public<T> void set(Address<T> key, T value) {
        data.put(key, value);
    }

    public <T> T get(Address<T> key) {
        Object value = data.get(key);
        return key.clazz().isInstance(value) ? key.clazz().cast(value) : null;
    }

    public <T> T computeIfAbsent(Address<T> key, Supplier<T> factory) {
        Object value = data.get(key);
        if (value == null) {
            value = factory.get();
            data.put(key, value);
        }
        return key.clazz().isInstance(value) ? key.clazz().cast(value) : null;
    }


    public<T> void remove(Address<T> key){
        data.remove(key);
    }

    // 其他快捷方法...
}
