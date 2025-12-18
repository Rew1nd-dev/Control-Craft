package com.verr1.controlcraft.foundation.data.terminal;

import com.verr1.controlcraft.foundation.type.descriptive.SlotType;
import net.createmod.catnip.data.Couple;

public record TerminalRowData(
        boolean enabled,
        SlotType type,
        double value,
        Couple<Double> min_max,
        boolean isBoolean,
        boolean isReversed
) {
}
