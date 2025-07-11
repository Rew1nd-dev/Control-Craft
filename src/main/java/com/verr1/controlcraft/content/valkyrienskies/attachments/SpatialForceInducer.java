package com.verr1.controlcraft.content.valkyrienskies.attachments;

import com.verr1.controlcraft.content.valkyrienskies.controls.InducerControls;
import com.verr1.controlcraft.foundation.data.logical.LogicalSpatial;
import org.jetbrains.annotations.NotNull;
import org.valkyrienskies.core.api.attachment.AttachmentHolder;
import org.valkyrienskies.core.api.ships.PhysShip;
import org.valkyrienskies.core.api.world.PhysLevel;

import java.util.function.Function;

public final class SpatialForceInducer extends ExpirableForceInducer<LogicalSpatial>{

    public static SpatialForceInducer getOrCreate(AttachmentHolder ship){
        return ship.getOrPutAttachment(SpatialForceInducer.class, SpatialForceInducer::new);
    }

    @Override
    protected void consume(
            @NotNull PhysShip physShip,
            @NotNull Function<Long, PhysShip> lookupPhysShip,
            @NotNull LogicalSpatial context,
            @NotNull PhysLevel world) {
        InducerControls.spatialTickControls(context, physShip);
    }
}
