package com.verr1.controlcraft.unstable.valkyrienskies.attachments;

import com.verr1.controlcraft.content.valkyrienskies.attachments.ExpirableForceInducer;
import com.verr1.controlcraft.content.valkyrienskies.controls.InducerControls;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.data.logical.LogicalAnchor;
import com.verr1.controlcraft.unstable.valkyrienskies.context.LogicalDirectionTarget;
import com.verr1.controlcraft.unstable.valkyrienskies.controls.AIControls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.valkyrienskies.core.api.attachment.AttachmentHolder;
import org.valkyrienskies.core.api.ships.PhysShip;
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.core.api.world.PhysLevel;

import java.util.function.Function;


public final class ConstantCruiseNavigator extends ExpirableForceInducer<LogicalDirectionTarget> {
    private static final LogicalAnchor NO_GRAVITY = new LogicalAnchor(0, -10, 0, WorldBlockPos.NULL, false, false, false);

    public static ConstantCruiseNavigator getOrCreate(AttachmentHolder ship){
        return ship.getOrPutAttachment(ConstantCruiseNavigator.class, ConstantCruiseNavigator::new);
    }


    @Override
    protected void consume(
            @NotNull PhysShip physShip,
            @NotNull Function<Long, @Nullable PhysShip> lookupPhysShip,
            @NotNull LogicalDirectionTarget context,
            PhysLevel world) {
        InducerControls.anchorTickControls(NO_GRAVITY, physShip);
        AIControls.rotateControl(
                physShip,
                context
        );
    }
}
