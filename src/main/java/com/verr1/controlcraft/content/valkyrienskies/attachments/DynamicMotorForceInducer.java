package com.verr1.controlcraft.content.valkyrienskies.attachments;

import com.verr1.controlcraft.content.valkyrienskies.controls.InducerControls;
import com.verr1.controlcraft.foundation.data.logical.LogicalDynamicMotor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.valkyrienskies.core.api.attachment.AttachmentHolder;
import org.valkyrienskies.core.api.ships.PhysShip;
import org.valkyrienskies.core.api.world.PhysLevel;

import java.util.Objects;
import java.util.function.Function;

public final class DynamicMotorForceInducer extends ExpirableForceInducer<LogicalDynamicMotor>{


    public static DynamicMotorForceInducer getOrCreate(AttachmentHolder ship){
          return ship.getOrPutAttachment(DynamicMotorForceInducer.class, DynamicMotorForceInducer::new);
//        var obj = ship.getAttachment(DynamicMotorForceInducer.class);
//        if(obj == null){
//            obj = new DynamicMotorForceInducer();
//            ship.saveAttachment(DynamicMotorForceInducer.class, obj);
//        }
//        return obj;
    }

    @Override
    protected void consume(
            @NotNull PhysShip physShip,
            @NotNull Function<Long, @Nullable PhysShip> lookupPhysShip,
            @NotNull LogicalDynamicMotor context,
            PhysLevel world
    ) {
        InducerControls.dynamicMotorTickControls(
                context,
                lookupPhysShip.apply(context.motorShipID()),
                physShip
        );
    }
}
