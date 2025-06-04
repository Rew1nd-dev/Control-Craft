package com.verr1.controlcraft.foundation.data.links;

import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;

public record ClientViewContext(BlockPos pos, String portName, Boolean isInput, Vec3 offset) {

    public BlockPort toPort(Level level){
        return new BlockPort(WorldBlockPos.of(level, pos), portName);
    }

}
