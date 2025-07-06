package com.verr1.controlcraft.content.valkyrienskies.attachments;

import com.verr1.controlcraft.content.valkyrienskies.controls.InducerControls;
import com.verr1.controlcraft.foundation.data.logical.LogicalFlap;
import kotlin.jvm.functions.Function1;
import org.jetbrains.annotations.NotNull;
import org.valkyrienskies.core.api.ships.PhysShip;
import org.valkyrienskies.core.api.ships.ServerShip;

import java.util.function.Function;

public final class FlapForceInducer extends ExpirableForceInducer<LogicalFlap>{


    public static FlapForceInducer getOrCreate(ServerShip ship){
        //return ship.getOrPutAttachment(AnchorForceInducer.class, AnchorForceInducer::new);
        var obj = ship.getAttachment(FlapForceInducer.class);
        if(obj == null){
            obj = new FlapForceInducer();
            ship.saveAttachment(FlapForceInducer.class, obj);
        }
        return obj;
    }

    @Override
    protected void consume(
            @NotNull PhysShip physShip,
            @NotNull Function<Long, PhysShip> lookupPhysShip,
            @NotNull LogicalFlap context
    ) {
        InducerControls.flapTickControls(context, physShip);
    }

}
