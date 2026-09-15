package com.verr1.controlcraft.content.links.computer;

import java.util.Locale;

public enum ComputerAuthorityMode {
    NONE,
    LEASED,
    FORCED;

    public String serializedName() {
        return name().toLowerCase(Locale.ROOT);
    }
}
