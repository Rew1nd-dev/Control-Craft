package com.verr1.controlcraft.foundation.cimulink.game;

import com.verr1.controlcraft.foundation.data.WorldBlockPos;

import java.util.List;
import java.util.Map;

public record PlantDiscoveryEntry(
        WorldBlockPos position,
        long shipId,
        String typeKey,
        String name,
        List<String> inputs,
        List<String> outputs,
        Map<String, Object> metadata
) {
}
