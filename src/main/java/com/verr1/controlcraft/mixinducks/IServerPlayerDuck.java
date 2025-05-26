package com.verr1.controlcraft.mixinducks;

import net.minecraft.core.SectionPos;

public interface IServerPlayerDuck {

    SectionPos controlcraft$lastSectionPos();

    void controlcraft$setLastSectionPos(SectionPos pos);

    SectionPos controlcraft$getAndSetLastSectionPos(SectionPos pos);
}
