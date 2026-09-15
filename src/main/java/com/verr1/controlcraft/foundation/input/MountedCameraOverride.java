package com.verr1.controlcraft.foundation.input;

import org.jetbrains.annotations.Nullable;
import org.joml.Vector3dc;
import org.spongepowered.asm.mixin.Unique;
import org.valkyrienskies.core.api.ships.ClientShip;

@Unique
public record MountedCameraOverride(
    @Nullable ClientShip clientShip,
    Vector3dc positionInShip,
    boolean thirdPerson,
    boolean transformRotation
) {
}
