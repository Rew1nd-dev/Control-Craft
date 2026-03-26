package com.verr1.controlcraft.content.valkyrienskies.attachments;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.verr1.controlcraft.content.valkyrienskies.controls.InducerControls;
import com.verr1.controlcraft.foundation.data.logical.LogicalFlap;
import com.verr1.controlcraft.foundation.vsapi.PhysShipWrapper;
import kotlin.jvm.functions.Function1;
import org.jetbrains.annotations.NotNull;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.ships.PhysShip;
import org.valkyrienskies.core.api.ships.ServerShip;

@JsonAutoDetect(
    fieldVisibility = JsonAutoDetect.Visibility.ANY,
    getterVisibility = JsonAutoDetect.Visibility.NONE,
    isGetterVisibility = JsonAutoDetect.Visibility.NONE,
    setterVisibility = JsonAutoDetect.Visibility.NONE
)
@JsonIgnoreProperties(ignoreUnknown = true)
public class FlapForceInducer extends ExpirableForceInducer<LogicalFlap>{

    @JsonIgnore
    private final Vector3d currentTickForces = new Vector3d();
    @JsonIgnore
    private final Vector3d currentTickTorque = new Vector3d();
    @JsonIgnore
    private final Vector3d lastTickForces = new Vector3d();
    @JsonIgnore
    private final Vector3d lastTickTorque = new Vector3d();

    public static FlapForceInducer getOrCreate(ServerShip ship){
        //return ship.getOrPutAttachment(AnchorForceInducer.class, AnchorForceInducer::new);
        var obj = ship.getAttachment(FlapForceInducer.class);
        if(obj == null){
            obj = new FlapForceInducer();
            ship.saveAttachment(FlapForceInducer.class, obj);
        }
        return obj;
    }

    public Vector3dc[] lastTickControl(){
        return new Vector3dc[]{new Vector3d(lastTickForces), new Vector3d(lastTickTorque)};
    }

    @Override
    protected void applyControlWithOther(@NotNull PhysShip physShip, @NotNull Function1<? super Long, ? extends PhysShip> lookupPhysShip) {
        currentTickTorque.zero();
        currentTickForces.zero();
        super.applyControlWithOther(physShip, lookupPhysShip);
        lastTickForces.set(currentTickForces);
        lastTickTorque.set(currentTickTorque);
    }

    @Override
    protected void consume(
            @NotNull PhysShip physShip,
            @NotNull Function1<? super Long, ? extends PhysShip> lookupPhysShip,
            @NotNull LogicalFlap context
    ) {
        Vector3dc[] ft = InducerControls.flapTickControls(context, PhysShipWrapper.of(physShip));
        if(ft == null)return;
        currentTickForces.add(ft[0]);
        currentTickTorque.add(ft[1]);
    }

}
