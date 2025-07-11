package com.verr1.controlcraft.registry;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.valkyrienskies.attachments.*;
import net.minecraftforge.fml.event.lifecycle.FMLLoadCompleteEvent;
import org.valkyrienskies.core.api.attachment.AttachmentHolder;
import org.valkyrienskies.core.api.attachment.AttachmentRegistration;
import org.valkyrienskies.core.api.events.ShipLoadEvent;
import org.valkyrienskies.core.api.ships.ShipForcesInducer;
import org.valkyrienskies.core.api.ships.ShipPhysicsListener;
import org.valkyrienskies.core.impl.hooks.VSEvents;
import org.valkyrienskies.mod.api.ValkyrienSkies;

import java.util.Arrays;

public enum ControlCraftAttachments {

    OBSERVER(Observer.class),
    QUEUE_FORCE_INDUCER(QueueForceInducer.class),

    ANCHOR(AnchorForceInducer.class),
    DYNAMIC_MOTOR(DynamicMotorForceInducer.class),
    SLIDER(DynamicSliderForceInducer.class),
    SPATIAL(SpatialForceInducer.class),
    JET(JetForceInducer.class),
    PROPELLER(PropellerForceInducer.class),
    FLAP(FlapForceInducer.class),
    KINEMATIC(KinematicMotorPoseInducer.class),



    ;

    public Class<?> getClazz() {
        return clazz;
    }

    private final Class<?> clazz;
    <T extends ShipPhysicsListener> ControlCraftAttachments (Class<T> clazz) {
        this.clazz = clazz;
    }

    public static void register(FMLLoadCompleteEvent event) {
        Arrays
            .stream(ControlCraftAttachments.values())
            .forEach(
                type -> ValkyrienSkies.api().registerAttachment(
                        ValkyrienSkies.api().newAttachmentRegistrationBuilder(type.clazz)
                                .useTransientSerializer()
                                .build()
                )
            );
    }


    public static void onShipLoad(ShipLoadEvent shipLoadEvent) {

        Arrays
            .stream(values())
            .forEach(
                    type -> {
                        try {
                            type.clazz
                                    .getMethod("getOrCreate", AttachmentHolder.class)
                                    .invoke(null, shipLoadEvent.getShip());
                        } catch (Exception e) {
                            ControlCraft.LOGGER.info(e.toString());
                        }
                    }
            );

    }


}
