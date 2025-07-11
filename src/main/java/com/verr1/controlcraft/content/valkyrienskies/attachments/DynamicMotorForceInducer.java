package com.verr1.controlcraft.content.valkyrienskies.attachments;

import com.verr1.controlcraft.content.valkyrienskies.controls.InducerControls;
import com.verr1.controlcraft.foundation.data.control.GroundBodyPhysShip;
import com.verr1.controlcraft.foundation.data.logical.LogicalDynamicMotor;
import org.jetbrains.annotations.NotNull;
import org.valkyrienskies.core.api.attachment.AttachmentHolder;
import org.valkyrienskies.core.api.ships.PhysShip;
import org.valkyrienskies.core.api.world.PhysLevel;

import java.util.Optional;
import java.util.function.Function;

public final class DynamicMotorForceInducer extends ExpirableForceInducer<LogicalDynamicMotor>{


    public static DynamicMotorForceInducer getOrCreate(AttachmentHolder ship){
        return ship.getOrPutAttachment(DynamicMotorForceInducer.class, DynamicMotorForceInducer::new);
    }

    @Override
    protected void consume(
            @NotNull PhysShip physShip,
            @NotNull Function<Long, PhysShip> lookupPhysShip,
            @NotNull LogicalDynamicMotor context,
            @NotNull PhysLevel world) {
        InducerControls.dynamicMotorTickControls(
                context,
                Optional.ofNullable(lookupPhysShip.apply(context.motorShipID())).orElse(GroundBodyPhysShip.INSTANCE),
                physShip
        );
    }
}
