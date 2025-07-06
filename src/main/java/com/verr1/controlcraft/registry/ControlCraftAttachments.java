package com.verr1.controlcraft.registry;

import com.verr1.controlcraft.content.valkyrienskies.attachments.*;
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




    // CAFFEINE(Caffeine.class),
    // KINEMATIC_MOTOR(KinematicMotorForceInducer.class),

    ;
    // I don't know where to register it, in constructor method will cause flw crash, idk
    // put it in serverStarting, and prevent duplicate registration
    public static boolean isRegistered = false;

    public Class<?> getClazz() {
        return clazz;
    }

    private final Class<?> clazz;
    <T extends ShipPhysicsListener> ControlCraftAttachments (Class<T> clazz) {
        this.clazz = clazz;
    }

    public static void register() {
        if(isRegistered) return;

        /*
        AttachmentRegistration<?> ANCHOR_INDUCER = ValkyrienSkies.api()
                .newAttachmentRegistrationBuilder(AnchorForceInducer.class)
                .useTransientSerializer()
                .build();

        AttachmentRegistration<?> OBSERVER = ValkyrienSkies.api()
                .newAttachmentRegistrationBuilder(Observer.class)
                .useTransientSerializer()
                .build();

        AttachmentRegistration<?> MOTOR = ValkyrienSkies.api()
                .newAttachmentRegistrationBuilder(MotorForceInducer.class)
                .useTransientSerializer()
                .build();

        AttachmentRegistration<?> QUEUE_FORCE_INDUCER = ValkyrienSkies.api()
                .newAttachmentRegistrationBuilder(QueueForceInducer.class)
                .useTransientSerializer()
                .build();

        AttachmentRegistration<?> SLIDER = ValkyrienSkies.api()
                .newAttachmentRegistrationBuilder(SliderForceInducer.class)
                .useTransientSerializer()
                .build();

        ValkyrienSkies.api().registerAttachment(ANCHOR_INDUCER);
        ValkyrienSkies.api().registerAttachment(OBSERVER);
        ValkyrienSkies.api().registerAttachment(MOTOR);
        ValkyrienSkies.api().registerAttachment(QUEUE_FORCE_INDUCER);
        ValkyrienSkies.api().registerAttachment(SLIDER);
        isRegistered = true;
        */

        Arrays
            .stream(ControlCraftAttachments.values())
            .forEach(
                type -> ValkyrienSkies.api().registerAttachment(
                        ValkyrienSkies.api().newAttachmentRegistrationBuilder(type.clazz)
                                .useTransientSerializer()
                                .build()
                )
            );


        isRegistered = true;
    }


    public static void onShipLoad(ShipLoadEvent shipLoadEvent) {
        Observer.getOrCreate(shipLoadEvent.getShip());
    }


}
