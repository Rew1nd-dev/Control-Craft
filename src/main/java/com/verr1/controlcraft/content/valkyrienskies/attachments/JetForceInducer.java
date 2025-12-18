package com.verr1.controlcraft.content.valkyrienskies.attachments;

import com.verr1.controlcraft.content.valkyrienskies.controls.InducerControls;
import com.verr1.controlcraft.foundation.data.logical.LogicalJet;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.valkyrienskies.core.api.attachment.AttachmentHolder;
import org.valkyrienskies.core.api.ships.PhysShip;
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.core.api.world.PhysLevel;

import java.util.function.Function;

public final class JetForceInducer extends ExpirableForceInducer<LogicalJet>{

    public static JetForceInducer getOrCreate(AttachmentHolder ship){
        return ship.getOrPutAttachment(JetForceInducer.class, JetForceInducer::new);
    }

    @Override
    protected void consume(@NotNull PhysShip physShip, @NotNull Function<Long, @Nullable PhysShip> lookupPhysShip, @NotNull LogicalJet context, PhysLevel world) {
        InducerControls.jetTickControls(
                context,
                physShip
        );
    }
}
