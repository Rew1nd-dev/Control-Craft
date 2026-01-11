package com.verr1.controlcraft.registry;

import com.verr1.controlcraft.content.valkyrienskies.attachments.*;
import com.verr1.controlcraft.unstable.valkyrienskies.attachments.AIBlockNetwork;
import com.verr1.controlcraft.unstable.valkyrienskies.attachments.ConstantCruiseNavigator;
import org.valkyrienskies.core.api.ships.ShipForcesInducer;
import org.valkyrienskies.core.api.ships.ShipPhysicsListener;
import org.valkyrienskies.core.impl.hooks.VSEvents;
import org.valkyrienskies.mod.api.ValkyrienSkies;

import java.util.Arrays;

public enum ControlCraftAttachments {

    OBSERVER(Observer.class),
    QUEUE_FORCE_INDUCER(QueueForceInducer.class),
    CIMULINK_BUS(CimulinkBus.class),
    CIMULINK_PORTS(CimulinkPorts.class),

    AI_BLOCK_NETWORK(AIBlockNetwork.class),
    NAVIGATOR(ConstantCruiseNavigator.class),

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
    <T> ControlCraftAttachments (Class<T> clazz) {
        this.clazz = clazz;
    }

    public static void register() {
        if(isRegistered) return;
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


    public static void onShipLoad(VSEvents.ShipLoadEvent shipLoadEvent) {
        Observer.getOrCreate(shipLoadEvent.getShip());
        /*
        Arrays
            .stream(ControlCraftAttachments.values())
            .forEach(
                type -> {
                    try {
                        type.clazz
                                .getMethod("getOrCreate", ServerShip.class)
                                .invoke(null, shipLoadEvent.getShip());
                    } catch (Exception e) {
                        ControlCraft.LOGGER.info(e.toString());
                    }
                }
            );
        * */

    }


}
