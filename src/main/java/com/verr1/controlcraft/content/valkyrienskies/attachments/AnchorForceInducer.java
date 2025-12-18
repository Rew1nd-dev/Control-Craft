package com.verr1.controlcraft.content.valkyrienskies.attachments;

import com.verr1.controlcraft.content.valkyrienskies.controls.InducerControls;
import com.verr1.controlcraft.foundation.data.logical.LogicalAnchor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.valkyrienskies.core.api.attachment.AttachmentHolder;
import org.valkyrienskies.core.api.ships.PhysShip;
import org.valkyrienskies.core.api.world.PhysLevel;

import java.util.function.Function;

public final class AnchorForceInducer extends ExpirableForceInducer<LogicalAnchor>{

    public static AnchorForceInducer getOrCreate(AttachmentHolder ship){
        return ship.getOrPutAttachment(AnchorForceInducer.class, AnchorForceInducer::new);
    }

    @Override
    protected void consume(@NotNull PhysShip physShip, @NotNull Function<Long, @Nullable PhysShip> lookupPhysShip, @NotNull LogicalAnchor context, PhysLevel world) {
        InducerControls.anchorTickControls(
                context,
                physShip
        );
    }
}
