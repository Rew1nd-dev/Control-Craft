package com.verr1.controlcraft.content.valkyrienskies.attachments;

import com.verr1.controlcraft.content.valkyrienskies.controls.InducerControls;
import com.verr1.controlcraft.foundation.data.logical.LogicalDynamicMotor;
import kotlin.jvm.functions.Function1;
import org.jetbrains.annotations.NotNull;
import org.valkyrienskies.core.api.attachment.AttachmentHolder;
import org.valkyrienskies.core.api.ships.PhysShip;
import org.valkyrienskies.core.api.ships.ServerShip;

import java.util.function.Function;

public final class DynamicMotorForceInducer extends ExpirableForceInducer<LogicalDynamicMotor>{


    public static DynamicMotorForceInducer getOrCreate(AttachmentHolder ship){
        return ship.getOrPutAttachment(DynamicMotorForceInducer.class, DynamicMotorForceInducer::new);
    }

    @Override
    protected void consume(
            @NotNull PhysShip physShip,
            @NotNull Function<Long, PhysShip> lookupPhysShip,
            @NotNull LogicalDynamicMotor context
    ) {
        InducerControls.dynamicMotorTickControls(
                context,
                lookupPhysShip.apply(context.motorShipID()),
                physShip
        );
    }
}
