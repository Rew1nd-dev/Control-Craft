package com.verr1.controlcraft.content.valkyrienskies.attachments;

import com.verr1.controlcraft.content.valkyrienskies.controls.InducerControls;
import com.verr1.controlcraft.foundation.data.control.GroundBodyPhysShip;
import com.verr1.controlcraft.foundation.data.logical.LogicalKinematicMotor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.valkyrienskies.core.api.attachment.AttachmentHolder;
import org.valkyrienskies.core.api.ships.PhysShip;
import org.valkyrienskies.core.api.world.PhysLevel;

import java.util.Optional;
import java.util.function.Function;

public final class KinematicMotorPoseInducer extends ExpirableForceInducer<LogicalKinematicMotor>{

    public static KinematicMotorPoseInducer getOrCreate(AttachmentHolder ship){
        return ship.getOrPutAttachment(KinematicMotorPoseInducer.class, KinematicMotorPoseInducer::new);
    }

    @Override
    protected void consume(
            @NotNull PhysShip physShip,
            @NotNull Function<Long, @Nullable PhysShip> lookupPhysShip,
            @NotNull LogicalKinematicMotor context,
            @NotNull PhysLevel world) {
        InducerControls.kinematicMotorTickControls(
                context,
                Optional.ofNullable(lookupPhysShip.apply(context.motorShipID())).orElse(GroundBodyPhysShip.INSTANCE),
                physShip
        );
    }
}
