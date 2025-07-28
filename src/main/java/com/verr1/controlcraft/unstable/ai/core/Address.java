package com.verr1.controlcraft.unstable.ai.core;

import java.util.Objects;

public record Address<T>(String key, Class<T> clazz) {


    @Override
    public boolean equals(Object object) {
        if (this == object) return true;
        if (!(object instanceof Address<?> address)) return false;
        return Objects.equals(key, address.key) && Objects.equals(clazz, address.clazz);
    }

    @Override
    public int hashCode() {
        return Objects.hash(key, clazz);
    }
}
