package com.verr1.controlcraft.foundation.cimulink.game;

import com.verr1.controlcraft.ControlCraft;
import net.minecraft.resources.ResourceLocation;

import java.util.Map;

public interface PlantMetadataProvider {

    default ResourceLocation plantType() {
        return ControlCraft.asResource("unknown");
    }

    default Map<String, Object> plantMetadata() {
        return Map.of();
    }
}
