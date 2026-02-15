package com.verr1.controlcraft.foundation.data;

import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.valkyrienskies.attachments.Observer;
import com.verr1.controlcraft.utils.VSAccessUtils;
import net.minecraft.world.phys.Vec3;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.ships.Ship;

public record ShipHitResult(
        Vec3 hitLocation,
        Ship ship
) {


    public Vector3dc getPosition(){
        Vector3dc p = ship.getTransform().getPositionInWorld();
        if(!ControlCraftServer.onMainThread()){
            return p;
        }
        long id = ship().getId();
        return VSAccessUtils.getShipOf(id)
                .map(Observer::getOrCreate)
                .map(Observer::read)
                .map(ShipPhysics::position)
                .orElse(p);
    }

    public Vector3dc getVelocity(){
        Vector3dc p = ship.getVelocity();
        if(!ControlCraftServer.onMainThread()){
            return p;
        }
        long id = ship().getId();
        return VSAccessUtils.getShipOf(id)
                .map(Observer::getOrCreate)
                .map(Observer::read)
                .map(ShipPhysics::velocity)
                .orElse(p);
    }

}