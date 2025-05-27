package com.verr1.controlcraft.foundation.camera;

import net.minecraft.core.BlockPos;
import net.minecraft.core.SectionPos;
import net.minecraft.world.phys.Vec3;

public class CameraMovementTracker {

    private final BlockPos cameraPos;
    private SectionPos lastSectionPos;

    public CameraMovementTracker(BlockPos cameraPos) {
        this.cameraPos = cameraPos;
        this.lastSectionPos = SectionPos.of(Vec3.ZERO);
    }

    public BlockPos cameraPos() {
        return cameraPos;
    }

    public SectionPos lastSectionPos() {
        return lastSectionPos;
    }

    public void setLastSectionPos(SectionPos lastSectionPos) {
        this.lastSectionPos = lastSectionPos;
    }
}
