package com.verr1.controlcraft.unstable.valkyrienskies.attachments;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.verr1.controlcraft.content.valkyrienskies.attachments.ExpirableForceInducer;
import com.verr1.controlcraft.content.valkyrienskies.attachments.JetForceInducer;
import com.verr1.controlcraft.content.valkyrienskies.controls.InducerControls;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.data.logical.LogicalAnchor;
import com.verr1.controlcraft.foundation.vsapi.PhysShipWrapper;
import com.verr1.controlcraft.unstable.pathing.LerpPathV2;
import com.verr1.controlcraft.unstable.valkyrienskies.context.LogicalDirectionTarget;
import com.verr1.controlcraft.unstable.valkyrienskies.context.LogicalLerpPathTarget;
import com.verr1.controlcraft.unstable.valkyrienskies.context.LogicalPathTarget;
import com.verr1.controlcraft.unstable.valkyrienskies.context.LogicalTarget;
import com.verr1.controlcraft.unstable.valkyrienskies.controls.AIControls;
import kotlin.jvm.functions.Function1;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.ships.PhysShip;
import org.valkyrienskies.core.api.ships.ServerShip;


public class ConstantCruiseNavigator extends ExpirableForceInducer<LogicalDirectionTarget> {
    private static final LogicalAnchor NO_GRAVITY = new LogicalAnchor(0, -10, 0, WorldBlockPos.NULL, false, false, false);

    public static ConstantCruiseNavigator getOrCreate(ServerShip ship){
        //return ship.getOrPutAttachment(AnchorForceInducer.class, AnchorForceInducer::new);
        var obj = ship.getAttachment(ConstantCruiseNavigator.class);
        if(obj == null){
            obj = new ConstantCruiseNavigator();
            ship.saveAttachment(ConstantCruiseNavigator.class, obj);
        }
        return obj;
    }


    @Override
    protected void consume(
            @NotNull PhysShip physShip,
            @NotNull Function1<? super Long, ? extends PhysShip> lookupPhysShip,
            @NotNull LogicalDirectionTarget context
    ) {
        InducerControls.anchorTickControls(NO_GRAVITY, PhysShipWrapper.of(physShip));
        AIControls.rotateControl(
                PhysShipWrapper.of(physShip),
                context
        );
    }
}
