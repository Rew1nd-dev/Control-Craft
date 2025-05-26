package com.verr1.controlcraft.foundation.data.logical;

import net.minecraft.core.BlockPos;
import org.joml.Vector3d;

public record LogicalFlap(BlockPos posInShip, Vector3d normal, double lift, double drag) {
}
