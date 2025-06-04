package com.verr1.controlcraft.foundation.data;

import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.phys.Vec3;

public record WandSelection(BlockPos pos, Direction face, Vec3 location) {
    public static final WandSelection NULL = new WandSelection(null, null, null);
}
