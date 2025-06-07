package com.verr1.controlcraft.foundation.data.links;

import java.util.List;

public record Coefficients(List<NamedCoeff> content) {
    public static final Coefficients EMPTY = new Coefficients(List.of());
}
