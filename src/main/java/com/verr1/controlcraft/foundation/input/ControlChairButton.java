package com.verr1.controlcraft.foundation.input;

public enum ControlChairButton {
    B0("b0"),
    B1("b1"),
    B2("b2"),
    B3("b3"),
    B4("b4"),
    B5("b5"),
    B6("b6"),
    B7("b7"),
    B8("b8"),
    B9("b9"),
    B10("b10"),
    B11("b11"),
    B12("b12"),
    B13("b13"),
    B14("b14"),
    B15("b15");

    public static final ControlChairButton[] VALUES = values();

    private final String serializedName;

    ControlChairButton(String serializedName) {
        this.serializedName = serializedName;
    }

    public long mask() {
        return 1L << ordinal();
    }

    public String serializedName() {
        return serializedName;
    }
}
