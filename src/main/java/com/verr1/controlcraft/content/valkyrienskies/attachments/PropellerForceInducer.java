package com.verr1.controlcraft.content.valkyrienskies.attachments;

import com.verr1.controlcraft.content.valkyrienskies.controls.InducerControls;
import com.verr1.controlcraft.foundation.data.logical.LogicalPropeller;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.valkyrienskies.core.api.attachment.AttachmentHolder;
import org.valkyrienskies.core.api.ships.PhysShip;
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.core.api.world.PhysLevel;

import java.util.function.Function;

public final class PropellerForceInducer extends ExpirableForceInducer<LogicalPropeller>{


    public static PropellerForceInducer getOrCreate(AttachmentHolder ship){
        return ship.getOrPutAttachment(PropellerForceInducer.class, PropellerForceInducer::new);
    }

    @Override
    protected void consume(@NotNull PhysShip physShip, @NotNull Function<Long, @Nullable PhysShip> lookupPhysShip, @NotNull LogicalPropeller context, PhysLevel world) {
        InducerControls.propellerTickControls(context, physShip);
    }
}
