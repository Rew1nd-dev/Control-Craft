package com.verr1.controlcraft.foundation.input;

public enum ControlChairAxis {
    YAW("lx"),
    PITCH("ry"),
    ROLL("rx"),
    THRUST("ly");

    public static final ControlChairAxis[] VALUES = values();

    private final String serializedName;

    ControlChairAxis(String serializedName) {
        this.serializedName = serializedName;
    }

    public String serializedName() {
        return serializedName;
    }
}
