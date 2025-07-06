package com.verr1.controlcraft.content.valkyrienskies.attachments;

import com.verr1.controlcraft.content.valkyrienskies.controls.InducerControls;
import com.verr1.controlcraft.foundation.data.logical.LogicalSpatial;
import kotlin.jvm.functions.Function1;
import org.jetbrains.annotations.NotNull;
import org.valkyrienskies.core.api.attachment.AttachmentHolder;
import org.valkyrienskies.core.api.ships.PhysShip;
import org.valkyrienskies.core.api.ships.ServerShip;

import java.util.function.Function;

public final class SpatialForceInducer extends ExpirableForceInducer<LogicalSpatial>{

    public static SpatialForceInducer getOrCreate(AttachmentHolder ship){
        return ship.getOrPutAttachment(SpatialForceInducer.class, SpatialForceInducer::new);
    }

    @Override
    protected void consume(
            @NotNull PhysShip physShip,
            @NotNull Function<Long, PhysShip> lookupPhysShip,
            @NotNull LogicalSpatial context
    ) {
        InducerControls.spatialTickControls(context, physShip);
    }
}
