package com.verr1.controlcraft.utils;

import com.verr1.controlcraft.content.valkyrienskies.attachments.CimulinkBus;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.PlantDiscoveryEntry;

import java.util.Locale;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public final class ConstraintClusterBusResolver {

    private ConstraintClusterBusResolver() {
    }

    public static Stream<CimulinkBus> buses(long shipOrGroundId) {
        return ConstraintClusterUtil.cachedClusterOf(shipOrGroundId)
                .stream()
                .map(ConstraintClusterUtil::getShipOf)
                .flatMap(Optional::stream)
                .map(CimulinkBus::getOrCreate);
    }

    public static Set<NamedComponent> access(long shipOrGroundId, String name) {
        if (name == null || name.isBlank()) {
            return Set.of();
        }
        return buses(shipOrGroundId)
                .flatMap(bus -> bus.access(name).stream())
                .collect(Collectors.toSet());
    }

    public static Set<String> allNames(long shipOrGroundId) {
        return buses(shipOrGroundId)
                .flatMap(bus -> bus.allNames().stream())
                .collect(Collectors.toSet());
    }

    public static Stream<PlantDiscoveryEntry> allPlants(long shipOrGroundId) {
        return ConstraintClusterUtil.cachedClusterOf(shipOrGroundId)
                .stream()
                .flatMap(shipId -> ConstraintClusterUtil.getShipOf(shipId)
                        .map(CimulinkBus::get)
                        .stream()
                        .flatMap(bus -> bus.allPlantEntries(shipId).stream()));
    }

    public static Stream<PlantDiscoveryEntry> findPlants(long shipOrGroundId, String typeKey) {
        if (typeKey == null || typeKey.isBlank()) {
            return Stream.empty();
        }

        String normalized = typeKey.toLowerCase(Locale.ROOT);
        return allPlants(shipOrGroundId)
                .filter(entry -> matchesType(entry.typeKey(), normalized));
    }

    public static double retrieve(long shipOrGroundId, String componentName, String port) {
        if (componentName == null || componentName.isBlank() || port == null || port.isBlank()) {
            return 0.0;
        }
        return access(shipOrGroundId, componentName)
                .stream()
                .filter(component -> component.hasOutput(port))
                .findFirst()
                .map(component -> component.peekOutput(port))
                .orElse(0.0);
    }

    public static void propagate(long shipOrGroundId, String componentName, String port, double value) {
        if (componentName == null || componentName.isBlank() || port == null || port.isBlank()) {
            return;
        }
        access(shipOrGroundId, componentName).forEach(component -> {
            if (component.hasInput(port)) {
                component.input(port, value);
            }
        });
    }

    private static boolean matchesType(String candidate, String normalizedQuery) {
        if (candidate == null) {
            return false;
        }

        String normalizedCandidate = candidate.toLowerCase(Locale.ROOT);
        if (Objects.equals(normalizedCandidate, normalizedQuery)) {
            return true;
        }

        int namespaceSeparator = normalizedCandidate.indexOf(':');
        return namespaceSeparator >= 0
                && Objects.equals(normalizedCandidate.substring(namespaceSeparator + 1), normalizedQuery);
    }
}
