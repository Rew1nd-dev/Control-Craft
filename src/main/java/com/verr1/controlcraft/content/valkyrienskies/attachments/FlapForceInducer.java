package com.verr1.controlcraft.content.valkyrienskies.attachments;

import com.verr1.controlcraft.content.valkyrienskies.controls.InducerControls;
import com.verr1.controlcraft.foundation.data.logical.LogicalFlap;
import org.jetbrains.annotations.NotNull;
import org.valkyrienskies.core.api.attachment.AttachmentHolder;
import org.valkyrienskies.core.api.ships.PhysShip;
import org.valkyrienskies.core.api.world.PhysLevel;

import java.util.function.Function;

public final class FlapForceInducer extends ExpirableForceInducer<LogicalFlap>{


    public static FlapForceInducer getOrCreate(AttachmentHolder ship){
        return ship.getOrPutAttachment(FlapForceInducer.class, FlapForceInducer::new);
    }

    @Override
    protected void consume(
            @NotNull PhysShip physShip,
            @NotNull Function<Long, PhysShip> lookupPhysShip,
            @NotNull LogicalFlap context,
            @NotNull PhysLevel world) {
        InducerControls.flapTickControls(context, physShip);
    }

}
