package com.verr1.controlcraft.foundation.data.terminal;

import net.createmod.catnip.data.Couple;
import org.joml.Vector2d;

public record TerminalRowSetting(Couple<Double> min_max, Boolean enabled, boolean isReversed) {
}
