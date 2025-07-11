package com.verr1.controlcraft.content.valkyrienskies.attachments;

import com.verr1.controlcraft.content.valkyrienskies.controls.InducerControls;
import com.verr1.controlcraft.foundation.data.control.GroundBodyPhysShip;
import com.verr1.controlcraft.foundation.data.logical.LogicalSlider;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.valkyrienskies.core.api.attachment.AttachmentHolder;
import org.valkyrienskies.core.api.ships.PhysShip;
import org.valkyrienskies.core.api.world.PhysLevel;

import java.util.Optional;
import java.util.function.Function;

public final class DynamicSliderForceInducer extends ExpirableForceInducer<LogicalSlider>{

    public static DynamicSliderForceInducer getOrCreate(AttachmentHolder ship){
        return ship.getOrPutAttachment(DynamicSliderForceInducer.class, DynamicSliderForceInducer::new);

    }

    @Override
    protected void consume(
            @NotNull PhysShip physShip,
            @NotNull Function<Long, @Nullable PhysShip> lookupPhysShip,
            @NotNull LogicalSlider context,
            @NotNull PhysLevel world) {
        InducerControls.sliderTickControls(
                context,
                Optional.ofNullable(lookupPhysShip.apply(context.selfShipID())).orElse(GroundBodyPhysShip.INSTANCE),
                physShip
        );
    }
}
